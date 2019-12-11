(ns exercises.adventofcode.d2
  (:require [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]
            [exercises.adventofcode.intcode-computer :as intcode-computer]))

(defn evaluate-memory [memory noun verb]
  (as-> (intcode-computer/get-empty-memory-map) it
        (assoc-in it [:memory] memory)
        (assoc-in it [:memory 1] noun)
        (assoc-in it [:memory 2] verb)
        (intcode-computer/evaluate it)))


(defn calculate-simple-result [file noun verb]
  (let [memory (aoc-io/day-2-input-from-file! file)]
    (as-> (evaluate-memory memory noun verb) it
          (nth (:memory it) 0))))

(defn found-expected? [memory expected]
  (= (nth (:memory memory) 0) expected))

(defn is-max? [value]
  (= value 99))

(defn loop-on-noun-and-verb [file expected]
  (let [memory (aoc-io/day-2-input-from-file! file)]
    (loop [noun 0, verb 0]
      (as-> memory current-memory
            (evaluate-memory current-memory noun verb)
            (if (found-expected? current-memory expected)
              (reduced {:noun noun, :verb verb})
              (if (is-max? verb)
                (if (is-max? noun)
                  (println "NOT FOUND")
                  (recur (inc noun) 0))
                (recur noun (inc verb))))))))

(defn calculate-complex-result [file expected]
  (let [result (deref (loop-on-noun-and-verb file expected))]
    (+ (* (:noun result) 100) (:verb result))))


(defn- evaluate-s1 [file]
  (calculate-simple-result file 12 2))

(defn- evaluate-s2 [file]
  (calculate-complex-result file 19690720))

(aoc-validation/validate-result-day2!
  (evaluate-s1 (aoc-io/day-file 2))
  (evaluate-s2 (aoc-io/day-file 2)))

