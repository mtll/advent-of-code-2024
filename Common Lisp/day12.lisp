(defpackage-plus-1:defpackage+ #:aoc2024.day12
  (:use #:cl #:series-utils #:aoc.utils)
  (:local-nicknames (#:a #:alexandria)
                    (#:s #:serapeum))
  (:export #:solve))

(in-package #:aoc2024.day12)

(s:eval-always (series-utils::install))

(defun parse (path)
  (loop :for line :in (uiop:read-file-lines path)
        :collect (append '(-1) (map 'list #'char-int line) '(-1))
          :into map
        :finally (let* ((pad (loop :repeat (length (car map)) :collect -1))
                        (init (append `(,pad) map `(,pad))))
                   (return (make-array (list (+ 2 (length map))
                                             (length (car map)))
                                       :element-type '(signed-byte 8)
                                       :initial-contents init)))))

(defun corner-sides (map r c)
  (loop :with curr = (abs (aref map r c))
        :for corner :in '((( 0 -1) (-1 -1) (-1  0))
                          ((-1  0) (-1  1) ( 0  1))
                          (( 1  0) ( 1  1) ( 0  1))
                          (( 1  0) ( 1 -1) ( 0 -1)))
        :for (x y z) = (s:mapply (s:op (abs (aref map (+ _ r) (+ _ c)))) corner)
        :count (or (and (/= x curr) (/= z curr))
                   (and (/= y curr) (= x z curr)))))

(defun fence (map row col)
  (loop :with stack = (list (list row col))
        :with region = (prog1 (aref map row col)
                         (setf (aref map row col) (- (aref map row col))))
        :with perimeter = 0
        :with sides = 0
        :for area :from 0
        :while stack
        :for (r c) = (pop stack)
        :do (loop :for (rn cn) :in (vn-nhd map r c)
                  :if (= region (aref map rn cn))
                    :do (push (list rn cn) stack)
                        (setf (aref map rn cn) (- region))
                  :else
                    :if (/= (- region) (aref map rn cn))
                      :do (incf perimeter))
            (incf sides (corner-sides map r c))
        :finally (return (values (* perimeter area) (* sides area)))))

(defun solve (path)
  (let ((map (parse path)))
    (gathering ((part1 collect-sum)
                (part2 collect-sum))
      (iterate ((point (scan-subscripts (array-dimensions map))))
        (when (> (apply #'aref map point) 0)
          (multiple-value-bind (p1 p2) (apply #'fence map point)
            (next-out part1 p1)
            (next-out part2 p2)))))))
