(ns exercises.adventofcode.day-5
  (:require [clojure.string :as str]
            [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]
            [exercises.adventofcode.intcode-computer :as intcode-computer]))

(defn day-5-input-from-file! [file-name]
  (->> (aoc-io/slurp-file! file-name)
       (str/trim-newline)
       (aoc-io/split-by-comma)
       (map aoc-io/cast-str-to-int)
       (into [])))

(def day-5-input (day-5-input-from-file! (aoc-io/day-file 5)))


(defn evaluate-result [memory input-val] "Star 1"
  (-> (intcode-computer/get-empty-memory-map)
      (assoc-in [:memory] memory)
      (assoc-in [:input] input-val)
      (intcode-computer/evaluate)))


(defn evaluate-s1 [memory]
  (last (:outputs (evaluate-result memory 1))))

(defn evaluate-s2 [memory]
  (last (:outputs (evaluate-result memory 5))))

(aoc-validation/validate-result-day5!
  (evaluate-s1 day-5-input)
  (evaluate-s2 day-5-input))

