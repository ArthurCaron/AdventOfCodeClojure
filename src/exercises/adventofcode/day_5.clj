(ns exercises.adventofcode.day-5
  (:require [clojure.string :as str]
            [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]
            [exercises.adventofcode.intcode-computer :as intcode-computer]))

(defn day-5-input-from-file! [file-name]
  (as-> file-name it
        (aoc-io/slurp-file! it)
        (str/trim-newline it)
        (aoc-io/split-by-comma it)
        (map aoc-io/cast-str-to-int it)
        (into [] it)))

(def day-5-input (day-5-input-from-file! (aoc-io/day-file 5)))


(defn evaluate-result [memory input-val] "Star 1"
  (as-> (intcode-computer/get-empty-memory-map) it
        (assoc-in it [:memory] memory)
        (assoc-in it [:input] input-val)
        (intcode-computer/evaluate it)))


(defn evaluate-s1 [memory]
  (last (:outputs (evaluate-result memory 1))))

(defn evaluate-s2 [memory]
  (last (:outputs (evaluate-result memory 5))))

(aoc-validation/validate-result-day5!
  (evaluate-s1 day-5-input)
  (evaluate-s2 day-5-input))

