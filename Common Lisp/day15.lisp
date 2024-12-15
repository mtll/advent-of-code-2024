(defpackage-plus-1:defpackage+ #:aoc2024.day15
  (:use #:cl #:series-utils #:aoc.utils)
  (:local-nicknames (#:a #:alexandria)
                    (#:s #:serapeum)
                    (#:pre #:cl-ppcre))
  (:export #:solve))

(in-package #:aoc2024.day15)

(s:eval-always (series-utils::install))

(defun parse (path)
  (destructuring-bind (map moves)
      (cl-ppcre:split "\\n\\n" (uiop:read-file-string path))
    (let* ((start nil))
      (values
       (list-to-array
        (collect 'list
          (mapping ((line (scan (cl-ppcre:split "\\n" map)))
                    (r (scan-range :from 0)))
            (collect 'list
              (mapping ((char (scan line))
                        (c (scan-range :from 0)))
                (case char
                  (#\# 1)
                  (#\. 0)
                  (#\O -2)
                  (#\@
                   (setf start (list r c))
                   0))))))
        '(signed-byte 8))
       start
       (gathering ((path collect))
         (iterate ((move (scan (subseq moves 0 (1- (length moves))))))
           (case move
             (#\> (next-out path '(0 1)))
             (#\< (next-out path '(0 -1)))
             (#\v (next-out path '(1 0)))
             (#\^ (next-out path '(-1 0))))))))))

(defun expand-map (map)
  (let ((new (collect 'list
               (mapping ((r (scan-range :below (array-dimension map 0))))
                 (collect-append
                  (mapping ((c (scan-range :below (array-dimension map 1))))
                    (case (aref map r c)
                      (1 '(1 1))
                      (0 '(0 0))
                      (-2 '(-2 2)))))))))
    (make-array (list (length new) (length (car new)))
                :element-type '(signed-byte 8)
                :initial-contents new)))

(defun push-box1 (map pos dir)
  (loop :with p1 = (mapcar #'+ pos dir)
        :for p = (mapcar #'+ dir p1) :then (mapcar #'+ dir p)
        :do (case (apply #'aref map p)
              (1
               (return pos))
              (0
               (rotatef (apply #'aref map p1)
                        (apply #'aref map p))
               (return p1)))))

(defun push-column (map pos dir)
  (labels ((parents (pt)
             (let ((next (mapcar #'+ pt dir)))
               (case (apply #'aref map next)
                 (-2 (list next (mapcar #'+ next '(0 1))))
                 (2 (list next (mapcar #'+ next '(0 -1))))
                 (1 (return-from push-column pos)))))
           (bfs (&rest pts)
             (when pts
               (apply #'bfs (delete-duplicates (mapcan #'parents pts) :test #'equal))
               (dolist (pt pts)
                 (rotatef (apply #'aref map pt)
                          (apply #'aref map (mapcar #'+ pt dir)))))))
    (bfs pos)
    (mapcar #'+ pos dir)))

(defun push-row (map pos dir)
  (loop :with next = (mapcar #'+ pos dir)
        :for row = (list next) :then (cons (mapcar #'+ (car row) dir) row)
        :do (case (apply #'aref map (car row))
              (1
               (return pos))
              (0
               (loop :for (a b) :on row :while b
                     :do (rotatef (apply #'aref map a)
                                  (apply #'aref map b)))
               (return next)))))

(defun push-box2 (map pos dir)
  (if (= 0 (car dir))
      (push-row map pos dir)
      (push-column map pos dir)))

(defun walk-path (push-fn map start path)
  (loop :with point = start
        :for move :in path
        :for next = (mapcar #'+ point move)
        :do (case (apply #'aref map next)
              (0 (setf point next))
              ((-2 2) (setf point (funcall push-fn map point move)))))
  map)

(defun score (map)
  (gathering ((p1 collect-sum))
    (iterate ((pt (scan-subscripts (array-dimensions map))))
      (when (= -2 (apply #'aref map pt))
        (destructuring-bind (r c) pt
          (next-out p1 (+ (* 100 r) c)))))))

(defun solve (path)
  (multiple-value-bind (map start path) (parse path)
    (let ((map2 (expand-map map)))
      (values (score (walk-path #'push-box1 map start path))
              (score (walk-path #'push-box2 map2 (mapcar #'* '(1 2) start) path))))))
