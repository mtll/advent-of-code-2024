(defpackage-plus-1:defpackage+ #:series-utils
  (:use #:cl)
  (:import-from #:serapeum #:op)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:s #:serapeum)
                    (#:nt #:named-readtables)
                    (#:pre #:cl-ppcre))
  (:export #:mapv
           #:mv->list
           #:take
           #:head
           #:collect-count-if
           #:scan-hash-keys
           #:scan-hash-values
           #:collect-radix
           #:collect-set
           #:scan-flattened
           #:scan-subscripts
           #:scan-re-groups
           #:collect-kv-pairs
           #:collect-tally
           #:scan-re-flines
           #:scan-permutations
           #:scan-subsets
           #:alist
           #:scan-subsequences
           #:collect-minimize
           #:collect-reduce))

(in-package #:series-utils)

(s:eval-always (series::install))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Additional reader macros

(defun parse-lambda-list-vars (lambda-list)
  (loop :for sym :in (a:flatten lambda-list)
        :unless (member sym '(&optional &rest &key &whole &aux &environment))
          :collect sym))

(defun |#D-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (let* ((form (read stream))
         (lambda-list (pop form))
         (series (pop form))
         (vars (parse-lambda-list-vars lambda-list)))
    `(,vars (map-fn '(values ,@(loop :repeat (length vars) :collect t))
                    (lambda (e)
                      (destructuring-bind ,lambda-list e
                        (values ,@vars)))
                    ,series))))

(defun |parse-#I-args| (args)
  (loop :with parsed = nil
        :with arity = 1
        :with apply = nil
        :for form :in args
        :do (cond ((numberp form)
                   (setf arity form))
                  ((and (symbolp form)
                        (string= (symbol-name form) "@"))
                   (setf apply t))
                  (t
                   (push (list apply
                               (if (= 1 arity)
                                   (gensym "ARG")
                                   (loop :repeat arity
                                         :collect (gensym "ARG")))
                               form)
                         parsed)
                   (setf arity 1)))
        :finally (return (reverse parsed))))

(defmacro \#I (fn &rest args)
  (let ((args (|parse-#I-args| args)))
    `(iterate ,(mapcar #'cdr args)
       (multiple-value-call ,(if (symbolp fn) `(function ,fn) fn)
         ,@(loop :for (apply args form) :in args
                 :if apply :collect `(values-list ,args)
                   :else :nconc (a:ensure-list args))))))

(defun |#I-reader| (stream subchar arg)
  (declare (ignore stream subchar arg))
  '\#I)

(defun |parse-#I-args| (args)
  (loop :with parsed = nil
        :with arity = 1
        :with apply = nil
        :for form :in args
        :do (assert (or (= arity 1) (null apply)))
            (cond ((numberp form)
                   (setf arity form))
                  ((and (symbolp form)
                        (string= (symbol-name form) "@"))
                   (setf apply t))
                  (t
                   (push (list apply
                               (if (= 1 arity)
                                   (gensym "ARG")
                                   (loop :repeat arity
                                         :collect (gensym "ARG")))
                               form)
                         parsed)
                   ;; (setf apply nil)
                   (setf arity 1)))
        :finally (return (reverse parsed))))

(defun |parse-#M-args| (args)
  (loop :with arity = 1
        :with apply = nil
        :with parsed = nil
        :for form :in args
        :do (assert (or (= arity 1) (null apply)))
            (cond ((numberp form)
                   (setf arity form))
                  ((and (symbolp form)
                        (string= (symbol-name form) "@"))
                   (setf apply t))
                  ((not (= 1 arity))
                   (let ((sym (gensym "ARG")))
                     (push `(,sym
                             (values-list ,sym)
                             (mv->list ,arity ,form))
                           parsed))
                   (setf arity 1))
                  (apply
                   (let ((sym (gensym "ARG")))
                     (push `(,sym
                             (values-list ,sym)
                             ,form)
                           parsed))
                   (setf apply nil))
                  (t
                   (let ((sym (gensym "ARG")))
                     (push `(,sym ,sym ,form) parsed))
                   (setf apply nil)))
        :finally (return (reverse parsed))))

(defun mapit (type fn args)
  (let ((parsed (|parse-#M-args| args)))
    `(map-fn ',type
             (lambda ,(mapcar #'first parsed)
               (multiple-value-call
                   ,(if (symbolp fn) `(function ,fn) fn)
                 ,@(mapcar #'second parsed)))
             ,@(mapcar #'third parsed))))

(defmacro |#M| (fn &rest args) (mapit t fn args))
(defmacro |#1M| (fn &rest args) (mapit t fn args))
(defmacro |#2M| (fn &rest args) (mapit '(values t t) fn args))
(defmacro |#3M| (fn &rest args) (mapit '(values t t t) fn args))
(defmacro |#4M| (fn &rest args) (mapit '(values t t t t) fn args))
(defmacro |#5M| (fn &rest args) (mapit '(values t t t t t) fn args))

(defun |#M-reader| (stream subchar arg)
  (declare (ignore stream subchar))
  (case arg
    ((nil) '|#M|)
    (1 '|#1M|)
    (2 '|#2M|)
    (3 '|#3M|)
    (4 '|#4M|)
    (5 '|#5M|)
    (t (error "The numeric argument to #M must be between 1 and 5 inclusive"))))

(nt:defreadtable :series-utils-readtable
  (:merge :standard)
  (:macro-char #\# :dispatch)
  (:dispatch-macro-char #\# #\D #'|#D-reader|)
  (:dispatch-macro-char #\# #\I #'|#I-reader|)
  (:dispatch-macro-char #\# #\M #'|#M-reader|)
  (:dispatch-macro-char #\# #\Z #'series::series-reader))

(defmacro install (&rest options)
  "Install series-utils readtable as well as run (series::install options)
Overrides :macro option"
  `(progn
     (series::install :macro nil ,@options)
     (named-readtables:in-readtable :series-utils-readtable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Collectors

(defmacro collect-radix (&rest things)
  (destructuring-bind (radix series &optional type)
      things
    (when type
      (rotatef series type))
    `(collect-fn ',(or type 'integer)
                 (lambda () 0)
                 (lambda (acc num)
                   (declare (type ,(or type 'integer) acc num))
                   (+ (* ,radix acc) num))
                 ,series)))

(defun collect-set (test series)
  (declare (optimizable-series-function 1))
  (let ((table (make-hash-table :test test)))
    (collect-fn 'list
                (lambda () nil)
                (lambda (set e)
                  (if (gethash e table)
                      set
                      (progn
                        (setf (gethash e table) t)
                        (cons e set))))
                series)))

(defun collect-count-if (series)
  (declare (optimizable-series-function 1))
  (collect-length (choose series (series t))))

(defmacro collect-kv-pairs (series &optional (test #'eql))
  (when test (rotatef series test))
  `(collect-fn 'hash-table
               (lambda () (make-hash-table :test ,test))
               (lambda (table pair)
                 (setf (gethash (car pair) table) (cadr pair))
                 table)
               ,series))

(defun collect-minimize (fn series)
  (declare (optimizable-series-function 1))
  (collect-fn 'list
              (lambda () nil)
              (lambda (e next)
                (let ((test (funcall fn next)))
                  (if (or (null e)
                          (< test (cdr e)))
                      (cons next test)
                      e)))
    series))

(defun collect-reduce (type val fn series)
  (declare (optimizable-series-function 1))
  (collect-fn type
              (lambda () val)
              (lambda (e next)
                (funcall fn e next))
              series))

(defmacro collect-tally (type test series)
  (ecase type
    (hash-table
     `(collect-fn 'hash-table
                  (lambda () (make-hash-table :test ,test))
                  (lambda (table item)
                    (setf (gethash item table)
                          (1+ (gethash item table 0)))
                    table)
                  ,series))
    (alist
     `(collect-fn 'list
                  (lambda () nil)
                  (lambda (alist item)
                    (a:if-let ((c (assoc item alist :test ,test)))
                      (progn
                        (incf (cdr c))
                        alist)
                      (acons item 1 alist)))
                  ,series))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scanners

(defun scan-subscripts (dimensions)
  (declare (optimizable-series-function 1))
  (map-fn 'list
          (lambda (i)
            (loop :for dim :in dimensions
                  :for coord = (mod i dim)
                  :do (setf i (floor i dim))
                  :collect coord))
          (scan-range :below (reduce #'* dimensions))))

(defun scan-flattened (list)
  (declare (optimizable-series-function 1))
  (let ((head (car list)))
    (scan-fn
     t
     (lambda ()
       (when list
         (prog1
             (car head)
           (setf head (cdr head)))))
     (lambda (_)
       (declare (ignore _))
       (when (null head)
         (setf list (cdr list))
         (setf head (car list)))
       (prog1
           (car head)
         (setf head (cdr head))))
     (lambda (_)
       (declare (ignore _))
       (and (null (car list))
            (null (cdr list)))))))

(defun scan-hash-values (hash-table)
  (declare (optimizable-series-function 1))
  (mapping (((k v) (scan-hash hash-table)))
    (declare (ignore k))
    v))

(defun scan-hash-keys (hash-table)
  (declare (optimizable-series-function 1))
  (mapping (((k v) (scan-hash hash-table)))
    (declare (ignore v))
    k))

(defun scan-re-groups (regex string &optional (start 0) end)
  (declare (optimizable-series-function 1))
  (let ((s (pre:create-scanner regex))
        (substr-end (or end (length string))))
    (multiple-value-bind (match-start match-end rg-start rg-end)
        (pre:scan s string :start start :end substr-end)
      (scan-fn-inclusive
       'list
       (lambda ()
         (loop :for start :across rg-start
               :for end :across rg-end
               :collect (when start (subseq string start end))
               :finally (multiple-value-setq
                            (match-start match-end rg-start rg-end)
                          (pre:scan s string :start match-end
                                             :end substr-end))))
       (lambda (_)
         (declare (ignore _))
         (loop :for start :across rg-start
               :for end :across rg-end
               :collect (subseq string start end)
               :finally (multiple-value-setq
                            (match-start match-end rg-start rg-end)
                          (pre:scan s string :start match-end
                                             :end substr-end))))
       (lambda (v)
         ;; (declare (ignore _))
         (or (null v)
             (null rg-start)))))))

(defmacro scan-re-flines (regex path)
  `(let ((s (pre:create-scanner ,regex)))
     (mapping ((string (scan-file ,path #'read-line)))
       (multiple-value-bind (b e rg-start rg-end)
           (pre:scan s string)
         (declare (ignore b e))
         (loop :for start :across rg-start
               :for end :across rg-end
               :when start
                 :collect (subseq string start end))))))

(defun scan-permutations (sequence &optional (copy t))
  (declare (optimizable-series-function 1))
  (let* ((length (length sequence))
         (c (make-array length :element-type 'fixnum))
         (i 0)
         (seq (if copy (copy-seq sequence) sequence)))
    (scan-fn
     t
     (lambda ()
       (if copy (copy-seq seq) seq))
     (lambda (_)
       (declare (ignore _))
       (loop :while (< i length) :do
         (cond
           ((>= (aref c i) i)
            (setf (aref c i) 0)
            (incf i))
           (t
            (if (evenp i)
                (rotatef (elt seq 0) (elt seq i))
                (rotatef (elt seq (aref c i)) (elt seq i)))
            (incf (aref c i))
            (setf i 0)
            (return (if copy
                        (copy-seq seq)
                        seq))))))
     (lambda (next)
       (null next)))))

(defmacro scan-subsets (type size seq)
  (case (eval type)
    (vector `(scan-subsets-vec ,size ,seq))
    (list `(scan-subsets-list ,size ,seq))
    (t (print type))))

;; Algorithm T, TAoCP 7.2.1.3
(defun scan-subsets-vec (size vec)
  (declare (optimizable-series-function 1))
  (let* ((j)
         (idx (make-array (+ 2 size)
                          :element-type 'a:array-index
                          :initial-contents (append (a:iota size)
                                                    `(,(length vec) 0)))))
    (scan-fn
     'list
     (lambda ()
       (setf j size)
       (do* ((i (1- size) (1- i))
             (r (list (aref vec (aref idx i)))
                (cons (aref vec (aref idx i)) r)))
            ((= i 0) r)))
     (lambda (_)
       (declare (ignore _))
       (prog ((x))
          (cond ((> j 0)
                 (setf x j)
                 (go t6))
                ((< (1+ (aref idx 0)) (aref idx 1))
                 (incf (aref idx 0))
                 (go visit))
                (t (setf j 2)))
        t4
          (setf (aref idx (- j 2)) (- j 2))
          (setf x (1+ (aref idx (1- j))))
          (cond ((= (aref idx j) x)
                 (incf j)
                 (go t4))
                ((> j size)
                 (go terminate)))
        t6
          (setf (aref idx (1- j)) x)
          (decf j)
        visit
          (return
            (do* ((i (1- size) (1- i))
                  (r (list (aref vec (aref idx i)))
                     (cons (aref vec (aref idx i)) r)))
                 ((= i 0) r)))
        terminate
          (return)))
     (lambda (prev)
       (null prev)))))

(defun scan-subsets-list (size list)
  (declare (optimizable-series-function 1))
  (let* ((idx (make-array
               (+ 2 size)
               :initial-contents (append
                                  (loop :for cons :on list
                                        :repeat size
                                        :collect cons)
                                  '(nil t))))
         (j))
    (scan-fn
     'list
     (lambda ()
       (setf j size)
       (do* ((i (1- size) (1- i))
             (r (list (car (aref idx i)))
                (cons (car (aref idx i)) r)))
            ((= i 0) r)))
     (lambda (_)
       (declare (ignore _))
       (prog ((x))
          (cond ((> j 0)
                 (setf x (nthcdr j list))
                 (go t6))
                ((not (eq (cdr (aref idx 0)) (aref idx 1)))
                 (pop (aref idx 0))
                 (go visit))
                (t (setf j 2)))
        t4
          (setf (aref idx (- j 2)) (nthcdr (- j 2) list))
          (setf x (cdr (aref idx (1- j))))
          (cond ((eq (aref idx j) x)
                 (incf j)
                 (go t4))
                ((> j size)
                 (go terminate)))
        t6
          (setf (aref idx (1- j)) x)
          (decf j)
        visit
          (return
            (do* ((i (1- size) (1- i))
             (r (list (car (aref idx i)))
                (cons (car (aref idx i)) r)))
            ((= i 0) r)))
        terminate
          (return)))
     (lambda (prev)
       (null prev)))))

(defmacro scan-subsequences (type size seq)
  (ecase type
    (list `(series-utils::scan-subsequences-list ,seq ,@(a:ensure-list size)))))

(defun scan-subsequences-list (seq &optional from to (by 1))
  (declare (optimizable-series-function 1))
  (let* ((queue (s:queue))
         (list (cons (car seq) (cdr seq))))
    (scan-fn
     'list
     (lambda ()
       (if (null to)
           (setf to from
                 from 1))
       (dotimes (i from)
         (s:enq (pop seq) queue))
       (copy-seq (s:qlist queue)))
     (lambda (prev)
       (declare (ignore prev))
       (if (null seq)
           (block nil
             (when (> (incf from by) to)
               (return nil))
             (setf seq list)
             (s:clear-queue queue)
             (dotimes (i from)
               (s:enq (pop seq) queue))
             (copy-seq (s:qlist queue)))
           (progn
             (s:enq (pop seq) queue)
             (s:deq queue)
             (copy-seq (s:qlist queue)))))
     (lambda (prev)
       (null prev)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mappers

(defmacro take (type seq &rest indices)
  (ecase (eval type)
    (list `(map-fn ,type
                   (let ((indices (sort ',indices #'<)))
                     (lambda (s)
                       (collect ,type
                         (choose (mask (scan indices)) (scan s)))))
                   (scan ,seq)))))

(defmacro head (n series)
  `(mapv ,n (lambda (list)
              (values-list (s:firstn ,n list)))
         ,series))

(defmacro mapv (&rest things)
  (destructuring-bind (size fn seq &optional type) things
    (when type (rotatef fn seq size type))
    `(map-fn ,(or type `'(values ,@(loop :repeat size :collect t)))
             ,(if (symbolp fn) `(function ,fn) fn)
             ,seq)))

(defmacro mv->list (size seq)
  (let ((vars (loop :repeat size :collect (gensym "VAR"))))
    `(mapping ((,(if (= 1 (length vars)) (car vars) vars) ,seq))
       (list ,@vars))))
