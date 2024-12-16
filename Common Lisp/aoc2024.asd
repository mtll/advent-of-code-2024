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
               :defpackage-plus)
  :components ((:file "series-utils")
               (:file "utils")
               (:file "day2")
               (:file "day3")
               (:file "day5")
               (:file "day7")
               (:file "day8")
               (:file "day9")
               (:file "day10")
               (:file "day12")
               (:file "day15")))
