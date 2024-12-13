(defpackage-plus-1:defpackage+ #:aoc2024.day12
  (:use #:cl #:series-utils #:aoc.utils)
  (:local-nicknames (#:a #:alexandria)
                    (#:s #:serapeum))
  (:export #:solve))

(in-package #:aoc2024.day12)

(s:eval-always (series-utils::install))

(defun parse (path)
  (loop :for line :in (uiop:read-file-lines path)
        :collect (append '(-1) (map 'list #'char-int line) '(-1)) :into map
        :finally (let* ((pad (loop :repeat (length (car map)) :collect -1)))
                   (return (make-array (list (+ 2 (length map))
                                             (length (car map)))
                                       :initial-contents `(,pad ,@map ,pad))))))

(defun corner-sides (map r c)
  (loop :with curr = (abs (aref map r c))
        :with (ul u ur l r dl d dr)
          = (s:mapply (s:op (abs (aref map _ _))) (moore-nhd map r c))
        :for (x y z) :in `((,l ,ul ,u) (,u ,ur ,r) (,r ,dr ,d) (,d ,dl ,l))
        :count (or (and (/= x curr) (/= z curr))
                   (and (/= y curr) (= x z curr)))))

(defun fence (map row col)
  (loop :with stack = (list (list row col)) :while stack
        :with region = (prog1 (aref map row col)
                         (setf (aref map row col) (- (aref map row col))))
        :for area :from 1
        :for (r c) = (pop stack)
        :sum (loop :for (rn cn) :in (vn-nhd map r c)
                   :if (= region (aref map rn cn))
                     :do (push (list rn cn) stack)
                         (setf (aref map rn cn) (- region))
                   :else :count (/= (- region) (aref map rn cn)))
          :into perimeter
        :sum (corner-sides map r c) :into sides
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
