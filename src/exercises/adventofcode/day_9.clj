(ns exercises.adventofcode.day-9
  (:require [clojure.string :as str]
            [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]))

(defn input-from-file! [file-name]
  (->> (aoc-io/slurp-file! file-name)
       (str/trim-newline)
       (aoc-io/split-by-line-return)))

(def input (input-from-file! (aoc-io/day-file 9)))





(defn evaluate-s1 [input])
(defn evaluate-s2 [input])

(aoc-validation/validate-result :9 :s1 (evaluate-s1 input))
(aoc-validation/validate-result :9 :s2 (evaluate-s2 input))
