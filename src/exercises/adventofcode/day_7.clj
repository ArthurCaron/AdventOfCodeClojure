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


(defn evaluate-result [memory inputs] "Star 1"
  (let [initial-state (-> (intcode-computer/get-empty-memory-map)
                          (assoc-in [:memory] memory))]
    (loop [memory initial-state]
      (let [new-state (intcode-computer/evaluate memory)
            stop-key (:stop-key new-state)
            memory-map (:memory-map new-state)]
        (cond
          (= stop-key "end") memory-map
          (= stop-key "input") (recur (assoc-in memory-map [:inputs] inputs))
          (= stop-key "output") (recur memory-map)
          )))))


(defn loop-eval
  ([memories phase-settings] (loop-eval memories phase-settings 0))
  ([memories phase-settings input]
   (loop [[current & remaining] phase-settings
          [current-mem & remaining-mem] memories
          last-output input
          all-results []]
     (if (nil? current)
       {:last-output last-output :all-results all-results}
       (let [evaluated-result (evaluate-result current-mem [current last-output])]
         (recur
           remaining
           remaining-mem
           (first (:outputs evaluated-result))
           (conj all-results evaluated-result)))))))

(defn do-perms [range memories]
  (reduce
    (fn [evaluations phase-settings-permutation]
      (->> (loop-eval memories phase-settings-permutation)
           (conj evaluations)))
    []
    (combinatorics/permutations range)))


(defn evaluate-s1 [memory]
  (->> (do-perms (range 0 5) (repeat 5 memory))
       (map :last-output)
       (apply max)))

(defn evaluate-s2 [memory]
  (let [first-result (do-perms (range 0 5) (repeat 5 memory))]
    (map :memory (:all-results (first first-result)))
    ;(apply max)
    ))

;(aoc-validation/validate-result :7 :s1 (evaluate-s1 input))
;(aoc-validation/validate-result :7 :s2 (evaluate-s2 input))