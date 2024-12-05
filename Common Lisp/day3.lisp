(defpackage-plus-1:defpackage+ #:aoc2024.day3
  (:use #:cl #:series-utils #:aoc.utils)
  (:local-nicknames (#:pre #:cl-ppcre)
                    (#:a #:alexandria)
                    (#:s #:serapeum))
  (:export #:solve))

(in-package #:aoc2024.day3)

(s:eval-always (series-utils::install))

(defun solve (path &aux (n 1) (str (uiop:read-file-string path)))
  (gathering ((part1 collect-sum)
              (part2 collect-sum))
    (pre:do-register-groups ((#'parse-integer a) (#'parse-integer b) c)
        ("(?:mul\\((\\d{1,3}),(\\d{1,3})\\))|(do\\(\\))|(don't\\(\\))" str)
      (cond (a (next-out part1 (* a b))
               (next-out part2 (* n a b)))
            (c (setf n 1))
            (t (setf n 0))))))
