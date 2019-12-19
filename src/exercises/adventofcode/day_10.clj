(ns exercises.adventofcode.day-10
  (:require [clojure.string :as str]
            [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]))

(defn input-from-file! [file-name]
  (->> (aoc-io/slurp-file! file-name)
       (str/trim-newline)
       (aoc-io/split-by-line-return)))

(def input (input-from-file! (aoc-io/day-file 10)))





(defn evaluate-s1 [input])
(defn evaluate-s2 [input])

(aoc-validation/validate-result :10 :s1 (evaluate-s1 input))
(aoc-validation/validate-result :10 :s2 (evaluate-s2 input))
