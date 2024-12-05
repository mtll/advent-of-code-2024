(defpackage-plus-1:defpackage+ #:aoc2024.day5
  (:use #:cl)
  (:local-nicknames (#:pre #:cl-ppcre))
  (:export #:solve))

(in-package #:aoc2024.day5)

(defun solve (path &aux (table (make-hash-table)))
  (destructuring-bind (rules lines)
      (cl-ppcre:split "\\n\\n" (uiop:read-file-string path))
    (pre:do-register-groups ((#'parse-integer a) (#'parse-integer b))
        ("(\\d+)\\|(\\d+)" rules)
      (push a (gethash b table)))
    (loop :with ord = (lambda (a b) (member b (gethash a table)))
          :with sorted = nil
          :for line :in (pre:split "\\n" lines)
          :for unsorted = nil
          :do (pre:do-matches-as-strings (a "\\d+" line)
                (push (parse-integer a) unsorted))
              (setf sorted (sort (copy-seq unsorted) ord))
          :if (equal unsorted sorted)
            :sum (nth (floor (length unsorted) 2) unsorted) :into part1
          :else
            :sum (nth (floor (length sorted) 2) sorted) :into part2
          :finally (return (values part1 part2)))))
