(defpackage-plus-1:defpackage+ #:aoc2024.day5
  (:use #:cl)
  (:local-nicknames (#:pre #:cl-ppcre))
  (:export #:solve))

(in-package #:aoc2024.day5)

(defun parse (path)
  (let ((table (make-hash-table)))
    (destructuring-bind (rules lines)
        (cl-ppcre:split "\\n\\n" (uiop:read-file-string path))
      (pre:do-register-groups ((#'parse-integer a)
                               (#'parse-integer b))
          ("(\\d+)\\|(\\d+)" rules)
        (push a (gethash b table)))
      (values (lambda (a b)
                (not (member b (gethash a table))))
              (loop :for line :in (pre:split "\\n" lines)
                    :for result = nil
                    :do (pre:do-matches-as-strings (a "\\d+" line)
                          (push (parse-integer a) result))
                    :collect (nreverse result))))))

(defun solve (path)
  (multiple-value-bind (ord lines) (parse path)
    (loop :for line :in lines
          :for sorted = (sort (copy-seq line) ord)
          :if (equal line sorted)
            :sum (nth (floor (length line) 2) line) :into part1
          :else
            :sum (nth (floor (length line) 2) sorted) :into part2
          :finally (return (values part1 part2)))))
