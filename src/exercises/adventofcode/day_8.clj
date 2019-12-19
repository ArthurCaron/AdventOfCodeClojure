(ns exercises.adventofcode.day-8
  (:require [clojure.string :as str]
            [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]))

(defn input-from-file! [file-name]
  (->> (aoc-io/slurp-file! file-name)
       (str/trim-newline)
       (aoc-io/split-by-line-return)))

(def input (input-from-file! (aoc-io/day-file 8)))





(defn evaluate-s1 [input])
(defn evaluate-s2 [input])

(aoc-validation/validate-result :8 :s1 (evaluate-s1 input))
(aoc-validation/validate-result :8 :s2 (evaluate-s2 input))
