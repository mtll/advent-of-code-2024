(defpackage-plus-1:defpackage+ #:aoc2024.day9
  (:use #:cl)
  (:export #:solve))

(in-package #:aoc2024.day9)

(defun parse (path)
  (loop :for c :across (uiop:read-file-string path)
        :for n = (digit-char-p c) :while n
        :for empty = nil :then (not empty)
        :for file = 0 :then (if empty file (1+ file))
        :nconc (loop :with i = (if empty -1 file) repeat n :collect i)
          :into init
        :finally (return (make-array (length init)
                                     :element-type '(signed-byte 16)
                                     :initial-contents init))))

(defun checksum (arr)
  (loop :for i :from 0
        :for n :across arr
        :when (> n 0) :sum (* n i)))

(defun part1 (disk)
  (loop :with arr = (copy-seq disk)
        :for head = (position -1 arr)
          :then (position -1 arr :start (1+ head))
        :for tail = (position-if (lambda (a) (>= a 0)) arr :from-end t)
          :then (position-if (lambda (a) (>= a 0)) arr :end tail :from-end t)
        :while (< head tail)
        :do (loop :while (< head tail)
                  :do (setf (aref arr head) (aref arr tail)
                            (aref arr tail) -1
                            head (position -1 arr :start head)
                            tail (1- tail)))
        :finally (return (checksum arr))))

(defun empty-ptrs (arr)
  (loop :with ptrs = (make-array 10 :initial-element nil)
        :for start :from 0 :below (length arr)
        :when (= (aref arr start) -1)
          :do (loop :for end :from (1+ start)
                    :while (and (< end (length arr))
                                (= (aref arr end) -1))
                    :finally (push start (aref ptrs (- end start)))
                             (setf start end))
        :finally (return (map 'vector #'nreverse ptrs))))

(defun insert-empty (empty-ptrs index start)
  (loop :with head = (cons -1 (aref empty-ptrs index))
        :for cons :on head
        :until (or (null (cdr cons))
                   (< (car cons) start (cadr cons)))
        :finally (setf (cdr cons) (cons start (cdr cons))
                       (aref empty-ptrs index) (cdr head))))

(defun move-file (arr empty-ptrs len from)
  (loop :with to = (length arr)
        :with to-len = (length empty-ptrs)
        :for i :from len :below (length empty-ptrs)
        :when (and (aref empty-ptrs i)
                   (< (car (aref empty-ptrs i)) to))
          :do (setf to (car (aref empty-ptrs i))
                    to-len i)
        :finally (when (< to from)
                   (setf (aref empty-ptrs to-len)
                         (cdr (aref empty-ptrs to-len)))
                   (loop :for j :from 0 :below len
                         :do (setf (aref arr (+ to j)) (aref arr (+ from j))
                                   (aref arr (+ from j)) -1))
                   (when (> (- to-len len) 0)
                     (insert-empty empty-ptrs (- to-len len) (+ to len))))))

(defun part2 (disk)
  (loop :with arr = (copy-seq disk)
        :with empty = (empty-ptrs arr)
        :for file :downfrom (find-if (lambda (n) (/= n -1)) arr :from-end t)
        :downto 1
        :for end = (position file arr :from-end t)
          :then (position file arr :from-end t :end (1+ start))
        :for start = (position-if (lambda (n) (/= n file)) arr
                                  :from-end t :end (1+ end))
        :do (move-file arr empty (- end start) (1+ start))
        :finally (return (checksum arr))))

(defun solve (path)
  (let ((disk (parse path)))
    (values (part1 disk) (part2 disk))))
