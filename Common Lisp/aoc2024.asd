(defsystem "aoc2024"
  :description "Advent of Code 2024"
  :author "David Feller"
  :license "GPL V3"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria
               :serapeum
               :cl-ppcre
               :series
               :named-readtables
               :cl-interpol
               :defpackage-plus)
  :components ((:file "series-utils")
               (:file "utils")
               (:file "day2")
               (:file "day3")
               (:file "day5")))
