(defpackage-plus-1:defpackage+ #:aoc2024.day10
  (:use #:cl #:series-utils #:aoc.utils)
  (:local-nicknames (#:a #:alexandria)
                    (#:s #:serapeum))
  (:export #:solve))

(in-package #:aoc2024.day10)

(s:eval-always (series-utils::install))

(defun parse (path)
  (loop :for line :in (uiop:read-file-lines path)
        :collect (map 'list #'digit-char-p line) :into map
        :finally (return (make-array (list (length map) (length (car map)))
                                     :element-type '(signed-byte 8)
                                     :initial-contents map))))

(defun all-positions (n array)
  (gathering ((ps collect))
    (iterate (#D((r c) (scan-subscripts (array-dimensions array))))
      (when (= n (aref array r c))
        (next-out ps (cons r c))))))

(defun score-trail (map row col)
  (loop :with map = (a:copy-array map)
        :with stack = (list (cons row col))
        :while stack
        :for (r . c) = (pop stack)
        :for curr = (aref map r c)
        :if (= curr 9)
          :sum 1
        :else :do
            (loop :for (rn . cn) :in (vn-nhd map r c)
                  :when (= (1+ curr) (aref map rn cn))
                    :do (push (cons rn cn) stack))
        :do (setf (aref map r c) -1)))

(defun score-trail2 (map row col)
  (loop :with stack = (list (cons row col))
        :while stack
        :for (r . c) = (pop stack)
        :for curr = (aref map r c)
        :if (= curr 9)
          :sum 1
        :else :do
          (loop :for (rn . cn) :in (vn-nhd map r c)
                :when (= (1+ curr) (aref map rn cn))
                  :do (push (cons rn cn) stack))))

(defun solve (map)
  (loop :for (r . c) :in (all-positions 0 map)
        :sum (score-trail map r c) :into part1
        :sum (score-trail2 map r c) :into part2
        :finally (return (values part1 part2))))

