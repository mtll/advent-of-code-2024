(defpackage-plus-1:defpackage+ #:aoc2024.day15
  (:use #:cl #:series-utils #:aoc.utils)
  (:local-nicknames (#:a #:alexandria)
                    (#:s #:serapeum)
                    (#:pre #:cl-ppcre))
  (:export #:solve))

(in-package #:aoc2024.day15)

(s:eval-always (series-utils::install))

(defun parse (path)
  (destructuring-bind (map moves) (pre:split "\\n\\n" (uiop:read-file-string path))
    (let* ((map2d (string2d map)))
      (values
       map2d
       (car (array-positions map2d #\@))
       (gathering ((path collect))
         (iterate ((move (scan (subseq moves 0 (1- (length moves))))))
           (case move
             (#\> (next-out path '(0 1)))
             (#\< (next-out path '(0 -1)))
             (#\v (next-out path '(1 0)))
             (#\^ (next-out path '(-1 0))))))))))

(defun expand-map (map)
  (list-to-array
   (collect 'list
     (mapping ((r (scan-range :below (array-dimension map 0))))
       (collect-append
        (mapping ((c (scan-range :below (array-dimension map 1))))
          (case (aref map r c)
            (#\# '(#\# #\#))
            (#\. '(#\. #\.))
            (#\O '(#\[ #\]))
            (#\@ '(#\@ #\.)))))))
   'character))

(defun push-column (map pos dir &aux (next (mapcar #'+ pos dir)))
  (labels ((parents (pt)
             (let ((next (mapcar #'+ pt dir)))
               (case (apply #'aref map next)
                 (#\[ (list next (mapcar #'+ next '(0 1))))
                 (#\] (list next (mapcar #'+ next '(0 -1))))
                 (#\# (return-from push-column pos)))))
           (bfs (&rest pts)
             (when pts
               (apply #'bfs (delete-duplicates (mapcan #'parents pts) :test #'equal))
               (dolist (pt pts next)
                 (rotatef (apply #'aref map pt)
                          (apply #'aref map (mapcar #'+ pt dir)))))))
    (bfs pos)))

(defun push-simple (map pos dir &aux (next (mapcar #'+ pos dir)))
  (just-do ((row (list next pos) (cons (mapcar #'+ (car row) dir) row)))
    (case (apply #'aref map (car row))
      (#\# (return-from push-simple pos))
      (#\. (loop :for (a b) :on row :while b
                 :do (rotatef (apply #'aref map a)
                              (apply #'aref map b))
                 :finally (return-from push-simple next))))))

(defun walk-path (push-fn map start path)
  (loop :for point = start :then (funcall push-fn map point move)
        :for move :in path
        :finally (return map)))

(defun score (map)
  (gathering ((p1 collect-sum))
    (iterate ((pt (scan-subscripts (array-dimensions map))))
      (let ((char (apply #'aref map pt)))
        (when (or (eq #\[ char) (eq #\O char))
          (next-out p1 (+ (* 100 (car pt)) (cadr pt))))))))

(defun solve (path)
  (s:mvlet* ((map start path (parse path))
             (map2 (expand-map map))
             (start2 (mapcar #'* '(1 2) start)))
    (values (score (walk-path #'push-simple map start path))
            (score (walk-path (lambda (map pos dir)
                                (if (= 0 (car dir))
                                    (push-simple map pos dir)
                                    (push-column map pos dir)))
                              map2 start2 path)))))
