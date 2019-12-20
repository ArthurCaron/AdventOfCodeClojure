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
      (assoc-in [:inputs] input-val)
      (intcode-computer/evaluate)))

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

(defn permutations-combination [range1 range2]
  (mapcat
    (fn [starting-permutation]
      (map #(into starting-permutation %) (combinatorics/permutations range2)))
    (combinatorics/permutations range1)))

(defn evaluate-s2 [memory]
  (let [first-result (do-perms (range 0 5) (repeat 5 memory))]
    (map :memory (:all-results (first first-result)))
    ;(apply max)
    ))

;(aoc-validation/validate-result :7 :s1 (evaluate-s1 input))
;(aoc-validation/validate-result :7 :s2 (evaluate-s2 input))