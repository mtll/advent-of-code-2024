(defpackage-plus-1:defpackage+ #:aoc2024.day20
  (:use #:cl #:series-utils #:aoc.utils)
  (:local-nicknames (#:a #:alexandria)
                    (#:s #:serapeum))
  (:export #:solve))

(in-package #:aoc2024.day20)

(s:eval-always (series-utils::install))

(defun find-path (map)
  (loop :with end = (car (array-positions map #\E))
        :with start = (car (array-positions map #\S))
        :for curr = start :then (find #\. (apply #'vn-nhd map curr)
                                      :key (a:curry #'apply #'aref map))
        :while curr
        :for i :from 0
        :do (setf (apply #'aref map curr) #\#)
        :collect (cons curr i) :into path
        :finally (return (append path (list (cons end (1+ i)))))))

(defun find-cheats (path &optional (distance 2))
  (gathering ((cheats collect))
    (iterate (#D(((from . from-dist) . rest) (scan-sublists path)))
      (iterate (#D((to . to-dist) (scan rest)))
        (a:when-let* ((d (taxicab-distance from to))
                      (_ (<= d distance))
                      (cheat (- to-dist from-dist d))
                      (_ (> cheat 0)))
          (next-out cheats cheat))))))

(defun solve (path)
  (let ((path (~> path uiop:read-file-string string2d find-path)))
    (values
     (collect-count-if
      (mapping ((n (scan (find-cheats path 2))))
        (>= n 100)))
     (collect-count-if
      (mapping ((n (scan (find-cheats path 20))))
        (>= n 100))))))
