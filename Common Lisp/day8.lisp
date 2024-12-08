(defpackage-plus-1:defpackage+ #:aoc2024.day8
  (:use #:cl #:series-utils)
  (:local-nicknames (#:s #:serapeum))
  (:export #:solve))

(in-package #:aoc2024.day8)

(s:eval-always (series-utils::install))

(defun parse (path)
  (loop :with elems := (make-hash-table)
        :for line :in (uiop:read-file-lines path)
        :for row :from 0
        :do (loop :for char :across line
                  :for col :from 0
                  :unless (char= char #\.)
                    :do (push (cons row col) (gethash char elems)))
        :finally (return (values elems (cons (1+ row) (length line))))))

(defun part1 (positions map bounds)
  (flet ((antinode (r1 c1 r2 c2)
           (let ((a (cons (- (* 2 r1) r2) (- (* 2 c1) c2))))
             (when (and (< -1 (car a) (car bounds))
                        (< -1 (cdr a) (cdr bounds)))
               (setf (gethash a map) t)))))
    (iterate (#D(((r1 . c1) (r2 . c2)) (scan-subsets 'list 2 positions)))
      (antinode r1 c1 r2 c2)
      (antinode r2 c2 r1 c1))))

(defun part2 (positions map bounds)
  (flet ((ray (r0 c0 dr dc)
           (loop :with (rmax . cmax) := bounds
                 :for r = r0 :then (+ r dr)
                 :for c = c0 :then (+ c dc)
                 :while (and (< -1 r rmax) (< -1 c cmax))
                 :do (setf (gethash (cons r c) map) t))))
    (iterate (#D(((r1 . c1) (r2 . c2)) (scan-subsets 'list 2 positions)))
      (ray r1 c1 (- r1 r2) (- c1 c2))
      (ray r2 c2 (- r2 r1) (- c2 c1)))))

(defun solve (path)
  (multiple-value-bind (elems bounds) (parse path)
    (loop :with map1 := (make-hash-table :test 'equal)
          :with map2 := (make-hash-table :test 'equal)
          :for positions :being :the :hash-values :of elems
          :do (part1 positions map1 bounds)
              (part2 positions map2 bounds)
          :finally (return (values (hash-table-count map1)
                                   (hash-table-count map2))))))
