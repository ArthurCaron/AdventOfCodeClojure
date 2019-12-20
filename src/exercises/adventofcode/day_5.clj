(ns exercises.adventofcode.day-5
  (:require [clojure.string :as str]
            [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]
            [exercises.adventofcode.intcode-computer :as intcode-computer]))

(defn input-from-file! [file-name]
  (->> (aoc-io/slurp-file! file-name)
       (str/trim-newline)
       (aoc-io/split-by-comma)
       (map aoc-io/cast-str-to-int)
       (into [])))

(def input (input-from-file! (aoc-io/day-file 5)))


(defn evaluate-result [memory input-val] "Star 1"
  (let [initial-state (-> (intcode-computer/get-empty-memory-map)
                          (assoc-in [:memory] memory))]
    (loop [memory initial-state]
      (let [new-state (intcode-computer/evaluate memory)
            stop-key (:stop-key new-state)
            memory-map (:memory-map new-state)]
        (cond
          (= stop-key "end") memory-map
          (= stop-key "input") (recur (assoc-in memory-map [:inputs] input-val))
          (= stop-key "output") (recur memory-map)
          )))))


(defn evaluate-s1 [input]
  (first (drop-while zero? (:outputs (evaluate-result input [1])))))

(defn evaluate-s2 [input]
  (first (drop-while zero? (:outputs (evaluate-result input [5])))))


(aoc-validation/validate-result :5 :s1 (evaluate-s1 input))
(aoc-validation/validate-result :5 :s2 (evaluate-s2 input))