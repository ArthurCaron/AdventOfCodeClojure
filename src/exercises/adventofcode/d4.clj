(ns exercises.adventofcode.d4
  (:require [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]))








(defn- evaluate-s1 [file])
(defn- evaluate-s2 [file])

(aoc-validation/validate-result-day4
  (evaluate-s1 (aoc-io/day-file 4))
  (evaluate-s2 (aoc-io/day-file 4)))
