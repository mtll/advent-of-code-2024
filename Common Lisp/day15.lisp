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
    (values
     (string2d map)
     (string2d map '((#\# . (#\# #\#))
                     (#\. . (#\. #\.))
                     (#\O . (#\[ #\]))
                     (#\@ . (#\@ #\.))))
     (gathering ((path collect))
       (iterate ((move (scan moves)))
         (case move
           (#\> (next-out path '(0 1)))
           (#\< (next-out path '(0 -1)))
           (#\v (next-out path '(1 0)))
           (#\^ (next-out path '(-1 0)))))))))

(defun move (map pos dir &aux (next (mapcar #'+ pos dir))
                           (column (/= 0 (car dir))))
  (labels ((parents (pt &aux (next (mapcar #'+ pt dir)))
             (case (apply #'aref map next)
               (#\[ `(,next ,@(when column `(,(mapcar #'+ next '(0 1))))))
               (#\] `(,next ,@(when column `(,(mapcar #'+ next '(0 -1))))))
               (#\O (list next))
               (#\# (return-from move pos))))
           (bfs (pts)
             (when pts
               (bfs (delete-duplicates (mapcan #'parents pts) :test #'equal))
               (dolist (pt pts next)
                 (rotatef (apply #'aref map pt)
                          (apply #'aref map (mapcar #'+ pt dir)))))))
    (bfs (list pos))))

(defun walk-path (map start path)
  (loop :for point = start :then (move map point move)
        :for move :in path
        :finally (return map)))

(defun score (map)
  (gathering ((score collect-sum))
    (iterate ((pt (scan-subscripts (array-dimensions map))))
      (let ((char (apply #'aref map pt)))
        (when (or (eq #\[ char) (eq #\O char))
          (next-out score (+ (* 100 (car pt)) (cadr pt))))))))

(defun solve (path)
  (s:mvlet* ((map map2 path (parse path))
             (start (car (array-positions map #\@)))
             (start2 (mapcar #'* '(1 2) start)))
    (values (score (walk-path map start path))
            (score (walk-path map2 start2 path)))))
