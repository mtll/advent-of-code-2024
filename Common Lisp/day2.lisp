(defpackage-plus-1:defpackage+ #:aoc2024.day2
  (:use #:cl)
  (:local-nicknames (#:pre #:cl-ppcre))
  (:export #:solve))

(in-package #:aoc2024.day2)

(defun safe-p (levels &optional (tolerance 0))
  (labels ((rec (p n tol)
             (cond ((null n) t)
                   ((or (null p) (<= 1 (- (car n) (car p)) 3))
                    (rec (cons (car n) p) (cdr n) tol))
                   ((> tol 0)
                    (or (rec p (cdr n) (1- tol))
                        (rec (cdr p) n (1- tol)))))))
    (or (rec nil levels tolerance)
        (rec nil (reverse levels) tolerance))))

(defun solve (path)
  (with-open-file (s (truename path))
    (loop :for line = (read-line s nil) :while line
          :for levels = (mapcar #'parse-integer (pre:all-matches-as-strings "(\\d+)" line))
          :count (safe-p levels) :into part1
          :count (safe-p levels 1) :into part2
          :finally (return (values part1 part2)))))
