(ns exercises.adventofcode.d5
  (:require [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]
            [exercises.adventofcode.intcode-computer :as intcode-computer]))

(defn evaluate-result [file input-val] "Star 1"
  (as-> (intcode-computer/get-empty-memory-map) it
        (assoc-in it [:memory] (aoc-io/day-5-input-from-file! file))
        (assoc-in it [:input] input-val)
        (intcode-computer/evaluate it)))


(defn evaluate-s1 [file]
  (last (:outputs (evaluate-result file 1))))

(defn evaluate-s2 [file]
  (last (:outputs (evaluate-result file 5))))

(aoc-validation/validate-result-day5!
  (evaluate-s1 (aoc-io/day-file 5))
  (evaluate-s2 (aoc-io/day-file 5)))

