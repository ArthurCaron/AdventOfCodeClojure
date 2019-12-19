(ns exercises.adventofcode.day-7
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combinatorics]
            [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]
            [exercises.adventofcode.intcode-computer :as intcode-computer]))

(defn input-from-file! [file-name]
  (->> (aoc-io/slurp-file! file-name)
       (str/trim-newline)
       (aoc-io/split-by-comma)
       (map aoc-io/cast-str-to-int)
       (into [])))

(def input (input-from-file! (aoc-io/day-file 7)))



(defn evaluate-result [memory input-val] "Star 1"
  (-> (intcode-computer/get-empty-memory-map)
      (assoc-in [:memory] memory)
      (assoc-in [:input] input-val)
      (intcode-computer/evaluate)))

(defn loop-eval [memory phase-settings]
  (loop [[current & remaining] phase-settings
         last-output 0]
    (if (nil? current)
      last-output
      (recur remaining (first (:outputs (evaluate-result memory [current last-output])))))))

(defn evaluate-s1 [memory]
  (->> (reduce
         (fn [evaluations phase-settings-permutation]
           (->> (loop-eval memory phase-settings-permutation)
                (conj evaluations)))
         []
         (combinatorics/permutations (range 0 5)))
  (apply max)))

(defn evaluate-s2 [memory]
  (:outputs (evaluate-result memory [0 0])))



(aoc-validation/validate-result :7 :s1 (evaluate-s1 input))
(aoc-validation/validate-result :7 :s2 (evaluate-s2 input))