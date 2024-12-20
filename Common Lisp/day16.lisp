(defpackage-plus-1:defpackage+ #:aoc2024.day16
  (:use #:cl #:series-utils #:aoc.utils)
  (:local-nicknames (#:a #:alexandria)
                    (#:s #:serapeum)
                    (#:pre #:cl-ppcre))
  (:export #:solve))

(in-package #:aoc2024.day16)

(defun find-path (map)
  (loop :with heap = (s:make-heap :key #'car :test #'<=)
          :initially (s:heap-insert heap (list 0 'e (car (array-positions map #\S))))
        :with seen = (make-hash-table :test 'equal)
        :with seats = nil
        :with best = -1
        :for (d dir p . path) = (s:heap-extract-maximum heap)
        :for (n e s w) = (apply #'vn-nhd map p)
        :when (> d best -1)
          :do (return (values best (length seats)))
        :unless (s:and-let* ((seenp (s:@ seen (cons p dir))))
                  (> d seenp))
          :do (setf (s:href seen (cons p dir)) d)
              (when (and (eq #\E (apply #'aref map p))
                         (or (= -1 best) (= d best)))
                (setf seats (union (list* p path) seats :test #'equal)
                      best d))
              (case dir
                ((e w)
                 (unless (eq #\# (apply #'aref map n))
                   (s:heap-insert heap (list* (+ d 1000) 'n p path)))
                 (unless (eq #\# (apply #'aref map s))
                   (s:heap-insert heap (list* (+ d 1000) 's p path))))
                ((s n)
                 (unless (eq #\# (apply #'aref map e))
                   (s:heap-insert heap (list* (+ d 1000) 'e p path)))
                 (unless (eq #\# (apply #'aref map w))
                   (s:heap-insert heap (list* (+ d 1000) 'w p path)))))
              (case dir
                (e (unless (eq #\# (apply #'aref map e))
                     (s:heap-insert heap (list* (1+ d) 'e e p path))))
                (w (unless (eq #\# (apply #'aref map w))
                     (s:heap-insert heap (list* (1+ d) 'w w p path))))
                (n (unless (eq #\# (apply #'aref map n))
                     (s:heap-insert heap (list* (1+ d) 'n n p path))))
                (s (unless (eq #\# (apply #'aref map s))
                     (s:heap-insert heap (list* (1+ d) 's s p path)))))))

(defun solve (path)
  (find-path (string2d (uiop:read-file-string path))))