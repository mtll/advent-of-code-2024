(defpackage-plus-1:defpackage+ #:aoc2024.day7
  (:use #:cl #:aoc-utils)
  (:local-nicknames (#:pre #:cl-ppcre)
                    (#:a #:alexandria))
  (:export #:solve))

(in-package #:aoc2024.day7)

(defun parse (path)
  (loop :for line :in (uiop:read-file-lines path)
        :collect (mapcar #'parse-integer (pre:split ":? " line))))

(defun detimes (a b)
  (multiple-value-bind (q r) (floor a b)
    (if (= r 0) q -1)))

(defun deconcat (a b)
  (multiple-value-bind (q r)
      (floor a (expt 10 (1+ (floor (log b 10)))))
    (if (= r b) q -1)))

(defun true-p (line ops)
  (labels ((rec (val nums ops)
             (cond
               ((< val 0) nil)
               ((= val 0) (not nums))
               (nums
                (some (lambda (op)
                        (rec (funcall op val (car nums))
                             (cdr nums)
                             ops))
                      ops)))))
    (if (rec (car line) (reverse (cdr line)) ops)
        (car line)
        0)))

(defun solve (path)
  (loop :with ops1 = (list #'detimes #'-)
        :with ops2 = (list #'deconcat #'detimes #'-)
        :for line :in (parse path)
        :sum (true-p line ops1) :into part1
        :sum (true-p line ops2) :into part2
        :finally (return (values part1 part2))))
