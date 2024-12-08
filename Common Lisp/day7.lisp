(defpackage-plus-1:defpackage+ #:aoc2024.day7
  (:use #:cl)
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

(defun decat (a b)
  (multiple-value-bind (q r)
      (floor a (expt 10 (1+ (floor (log b 10)))))
    (if (= r b) q -1)))

(defun true-p (val nums ops)
  (cond
    ((< val 0) nil)
    ((= val 0) (not nums))
    (nums
     (loop :for op :in ops
           :thereis (true-p (funcall op val (car nums)) (cdr nums) ops)))))

(defun solve (path)
  (loop :with ops1 = (list #'detimes #'-)
        :with ops2 = (list #'decat #'detimes #'-)
        :for (val . nums) :in (parse path)
        :when (true-p val (reverse nums) ops1) :sum val :into part1
        :when (true-p val (reverse nums) ops2) :sum val :into part2
        :finally (return (values part1 part2))))
