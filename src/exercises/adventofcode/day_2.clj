(ns exercises.adventofcode.day-2
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

(def input (input-from-file! (aoc-io/day-file 2)))


(defn evaluate-memory [memory noun verb]
  (-> (intcode-computer/get-empty-memory-map)
      (assoc-in [:memory] memory)
      (assoc-in [:memory 1] noun)
      (assoc-in [:memory 2] verb)
      (intcode-computer/evaluate)))


(defn calculate-simple-result [memory noun verb]
  (let [evaluated-memory (evaluate-memory memory noun verb)]
    (nth (:memory evaluated-memory) 0)))

(defn found-expected? [memory expected]
  (= (nth (:memory memory) 0) expected))

(defn is-max? [value]
  (= value 99))

(defn loop-on-noun-and-verb [memory expected]
  (loop [noun 0, verb 0]
    (let [evaluated-memory (evaluate-memory memory noun verb)]
      (if (found-expected? evaluated-memory expected)
        (reduced {:noun noun, :verb verb})
        (if (is-max? verb)
          (if (is-max? noun)
            (println "NOT FOUND")
            (recur (inc noun) 0))
          (recur noun (inc verb)))))))

(defn calculate-complex-result [input expected]
  (let [result (deref (loop-on-noun-and-verb input expected))]
    (+ (* (:noun result) 100) (:verb result))))


(defn evaluate-s1 [input]
  (calculate-simple-result input 12 2))

(defn evaluate-s2 [input]
  (calculate-complex-result input 19690720))

(aoc-validation/validate-result :2 :s1 (evaluate-s1 input))
(aoc-validation/validate-result :2 :s2 (evaluate-s2 input))

