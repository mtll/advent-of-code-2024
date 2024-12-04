(defpackage-plus-1:defpackage+ #:aoc2024.day3
  (:use #:cl #:series-utils #:aoc.utils)
  (:local-nicknames (#:pre #:cl-ppcre)
                    (#:a #:alexandria))
  (:export #:solve))

(in-package #:aoc2024.day3)

(s:eval-always (series-utils::install))

(defun solve (path &aux (str (uiop:read-file-string path)))
  (let ((start)
        (end 0)
        (part1 0)
        (part2 0))
    (iterate ((re (series "(?s).*?(?:don't\\(\\)|$)" "(?s).*?(?:do\\(\\)|$)"))
              (n (series 1 0)))
      (multiple-value-setq (start end) (pre:scan re str :start end))
      (unless (> end start) (return-from solve (values part1 part2)))
      (pre:do-register-groups ((#'parse-integer a) (#'parse-integer b))
          ("mul\\((\\d{1,3}),(\\d{1,3})\\)" str nil :start start :end end :sharedp t)
        (incf part1 (* a b))
        (incf part2 (* n (* a b)))))))
