(defpackage-plus-1:defpackage+ #:aoc.utils
  (:use #:cl #:series-utils)
  (:import-from #:serapeum #:op)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:s #:serapeum)
                    (#:pre #:cl-ppcre))
  (:export #:~>
           #:~>>
           #:call
           #:abbr
           #:if-let*
           #:unordered-removef
           #:unordered-removef-pos
           #:vn-nhd
           #:moore-nhd
           #:with-timing
           #:just-do
           #:square-vn-nhd
           #:bool
           #:split-lambda-list
           #:defun/type
           #:defun/type/inln
           #:defun/inln
           #:list-to-array
           #:string2d
           #:array-positions))

(in-package #:aoc.utils)

(s:eval-always (series-utils::install))

;;; From rutils
(define-condition utils-style-warning (simple-condition style-warning) ())

(defmacro abbr (short long &optional lambda-list)
  "Abbreviate LONG macro or function name as SHORT.
   If LAMBDA-LIST is present, also copy appropriate SETF-expander."
  `(s:eval-always
     ;; Lispworks signals error while abbreviating to keywords
     ;; SBCL has package locks when accessing built-in functionality
     ;; other similar things are probably possible in other implementations
     (handler-bind ((error (lambda (e)
                             (let ((r (find-restart 'continue e)))
                               (when r
                                 (warn 'utils-style-warning
                                       :format-control
                                       "Skipped error during abbreviation: ~A"
                                       :format-arguments (list e))
                                 (invoke-restart r))))))
       (cond
         ((macro-function ',long)
          (setf (macro-function ',short) (macro-function ',long))
          #+ccl (setf (ccl:arglist ',short) (ccl:arglist ',long)))
         ((special-operator-p ',long)
          (error "Can't abbreviate a special-operator ~a" ',long))
         ((fboundp ',long)
          (setf (fdefinition ',short) (fdefinition ',long))
          #+ccl (setf (ccl:arglist ',short) (ccl:arglist ',long))
          ,(when lambda-list
             `(define-setf-expander ,short ,lambda-list
                (values ,@(multiple-value-bind
                                (dummies vals store store-form access-form)
                              (get-setf-expansion
                               (cons long (remove-if (lambda (sym)
                                                       (member sym '(&optional &key)))
                                                     lambda-list)))
                            (let ((expansion-vals (mapcar (lambda (x) `(quote ,x))
                                                          (list dummies
                                                                vals
                                                                store
                                                                store-form
                                                                access-form))))
                              (setf (second expansion-vals)
                                    (cons 'list vals))
                              expansion-vals))))))
         (t
          (error "Can't abbreviate ~a" ',long)))
       (setf (documentation ',short 'function) (documentation ',long 'function))
       ',short)))

(abbr ~> alexandria-2:line-up-first)
(abbr ~>> alexandria-2:line-up-last)

(defmacro call (fn &rest args)
  (let ((parsed))
    (loop :with apply = nil
          :for arg :in args
          :do (cond ((and (symbolp arg)
                          (string= (symbol-name arg) "@"))
                     (setf apply t))
                    (apply
                     (push `(values-list ,arg) parsed)
                     (setf apply nil))
                    (t (push arg parsed))))
    `(multiple-value-call ,fn ,@(reverse parsed))))

(defun unordered-removef (item vector &key (test #'eql))
  (a:if-let ((pos (position item vector :test test)))
    (setf (aref vector pos)
          (aref vector (1- (length vector))))
    (decf (fill-pointer vector))))

(defun unordered-removef-pos (vector position)
  (setf (aref vector position)
        (aref vector (1- (length vector))))
  (decf (fill-pointer vector)))

(defun vn-nhd (array r c)
  (declare (type (array * 2) array)
           (type a:array-index r c))
  (let* ((r+1 (1+ r))
         (r-1 (1- r))
         (c+1 (1+ c))
         (c-1 (1- c)))
    (gathering ((nhd collect))
      (when (array-in-bounds-p array r-1 c)
        (next-out nhd (list r-1 c)))
      (when (array-in-bounds-p array r c+1)
        (next-out nhd (list r c+1)))
      (when (array-in-bounds-p array r+1 c)
        (next-out nhd (list r+1 c)))
      (when (array-in-bounds-p array r c-1)
        (next-out nhd (list r c-1))))))

(defun flat-vn-nhd (side-len pt)
  (declare (type a:array-index pt))
  (let* ((r+1 (+ pt side-len))
         (r-1 (- pt side-len))
         (c+1 (1+ pt))
         (c-1 (1- pt)))
    (gathering ((nhd collect))
      (unless (= (1- side-len) (mod pt side-len))
        (next-out nhd c+1))
      (unless (= 0 (mod pt side-len))
        (next-out nhd c-1))
      (when (< r+1 (expt side-len 2))
        (next-out nhd r+1))
      (when (>= r-1 0)
        (next-out nhd r-1)))))

(defun moore-nhd (array r c)
  (declare (type (array * 2) array)
           (type a:array-index r c))
  (gathering ((nhd collect))
    (iterate ((pt (#M(s:op (list (+ r (car _1))
                                 (+ c (cdr _1))))
                     (scan '((-1 . -1) (-1 .  0) (-1 .  1)
                             ( 0 . -1)           ( 0 .  1)
                             ( 1 . -1) ( 1 .  0) ( 1 .  1))))))
      (when (array-in-bounds-p array (car pt) (cadr pt))
        (next-out nhd pt)))))

(defmacro with-timing ((elapsed-symbol) &body body)
  (let ((t0 (gensym "TIMER")))
    `(let ((,t0 (get-internal-real-time)))
       (symbol-macrolet
           ((,elapsed-symbol
              (float (/ (- (get-internal-real-time) ,t0)
                        internal-time-units-per-second))))
         ,@body))))

(defmacro just-do (varlist &body body)
  (s:mvlet* ((body declarations (a:parse-body body))
             (begin (gensym "BEGIN"))
             (name (when (eq (car body) :named)
                     (cadr body)))
             (body (if name (cddr body) body)))
    `(block ,name
       (let* ,(mapcar (op (list (car _1) (cadr _1)))
               varlist)
         ,@declarations
         (tagbody
            ,begin
            ,@body
            ,@(loop :with output = nil
                    :for form :in varlist
                    :for step = (caddr form)
                    :when step :do
                      (if (and (symbolp step)
                               (equal (symbol-name step) "_"))
                          (push `(setf ,(car form) ,(cadr form))
                                output)
                          (push `(setf ,(car form) ,step)
                                output))
                    :finally (return (reverse output)))
            (go ,begin))))))

(defun bool (p)
  (if p 1 0))

(defun split-lambda-list (lambda-list)
  (loop for cons on lambda-list
        for tail = (cdr cons)
        until (member (car tail) lambda-list-keywords)
        finally (when cons (rplacd cons nil))
                (return (values lambda-list tail))))

(defmacro defun/inln (name lambda-list &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list ,@body)))

(defmacro defun/type (name typed-lambda-list return-type &body body)
  (multiple-value-bind (typed-req rest)
      (split-lambda-list typed-lambda-list)
    (multiple-value-bind (body declarations doc-string)
        (a:parse-body body :documentation t)
      (let ((req (mapcar (lambda (a) (if (consp a) (car a) a)) typed-req))
            (req-types (mapcar (lambda (a) (if (consp a) (cadr a) t)) typed-req))
            (rest-types (mapcar (lambda (a)
                                  (if (member a lambda-list-keywords) a t))
                                rest)))
        `(progn
           (declaim (ftype (function ,(append req-types rest-types) ,return-type)
                           ,name))
           (defun ,name ,(concatenate 'list req rest)
             ,@(append (when doc-string (list doc-string))
                      declarations
                      (loop for r in req
                            for type in req-types
                            unless (eq type t)
                              collect `(declare (type ,type ,r))))
             (the ,return-type (progn ,@body))))))))

(defmacro defun/type/inln (name typed-lambda-list return-type &body body)
  `(progn
     (declaim (inline ,name))
     (defun/type ,name ,typed-lambda-list ,return-type ,@body)))

(defun list-to-array (list &optional (element-type t))
  (make-array (loop :for l = list :then (car l)
                    :while (listp l)
                    :collect (length l))
              :element-type element-type
              :initial-contents list))

(defun string2d (string &optional rules)
  (list-to-array
   (loop :for line :in (pre:split "\\n" string)
         :collect (a:flatten (loop :for char :across line
                                   :collect (or (cdr (assoc char rules))
                                                char))))
   'character))

(defun array-positions (array item &key (test #'eql))
  (gathering ((ps collect))
    (iterate ((pt (scan-subscripts (array-dimensions array))))
      (when (funcall test item (apply #'aref array pt))
        (next-out ps pt)))))
