(defpackage-plus-1:defpackage+ #:aoc2024.day2
  (:use #:cl)
  (:export #:solve))

(in-package #:aoc2024.day2)

(defun safe-p (diffs &optional (tolerance 0))
  (labels ((unsafe (p d n tol)
             (cond ((null d) nil)
                   ((<= 1 d 3) (unsafe (cons d p) (car n) (cdr n) tol))
                   ((> tol 0)
                    (when n
                      (and (unsafe p (+ d (car n)) (cdr n) (1- tol))
                           (if p
                               (unsafe (cdr p) (+ d (car p)) n (1- tol))
                               (unsafe nil (car n) (cdr n) (1- tol))))))
                   (t t))))
    (or (not (unsafe nil (car diffs) (cdr diffs) tolerance))
        (not (unsafe nil (- (car diffs)) (mapcar #'- (cdr diffs)) tolerance)))))

(defun solve (path)
  (with-open-file (s (truename path))
    (loop :for line = (read-line s nil) :while line
          :for diff = nil
          :for prev = nil
          :do (cl-ppcre:do-register-groups ((#'parse-integer i)) ("(\\d+)" line)
                (when prev (push (- i prev) diff))
                (setf prev i))
          :count (safe-p diff) :into part1
          :count (safe-p diff 1) :into part2
          :finally (return (values part1 part2)))))

