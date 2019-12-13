(ns exercises.adventofcode.day-2
  (:require [clojure.string :as str]
            [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]
            [exercises.adventofcode.intcode-computer :as intcode-computer]))

(defn day-2-input-from-file! [file-name]
  (as-> file-name it
        (aoc-io/slurp-file! it)
        (str/trim-newline it)
        (aoc-io/split-by-comma it)
        (map aoc-io/cast-str-to-int it)
        (into [] it)))

(def day-2-input (day-2-input-from-file! (aoc-io/day-file 2)))


(defn evaluate-memory [memory noun verb]
  (as-> (intcode-computer/get-empty-memory-map) it
        (assoc-in it [:memory] memory)
        (assoc-in it [:memory 1] noun)
        (assoc-in it [:memory 2] verb)
        (intcode-computer/evaluate it)))


(defn calculate-simple-result [memory noun verb]
  (as-> (evaluate-memory memory noun verb) it
          (nth (:memory it) 0)))

(defn found-expected? [memory expected]
  (= (nth (:memory memory) 0) expected))

(defn is-max? [value]
  (= value 99))

(defn loop-on-noun-and-verb [memory expected]
  (loop [noun 0, verb 0]
      (as-> memory current-memory
            (evaluate-memory current-memory noun verb)
            (if (found-expected? current-memory expected)
              (reduced {:noun noun, :verb verb})
              (if (is-max? verb)
                (if (is-max? noun)
                  (println "NOT FOUND")
                  (recur (inc noun) 0))
                (recur noun (inc verb)))))))

(defn calculate-complex-result [input expected]
  (let [result (deref (loop-on-noun-and-verb input expected))]
    (+ (* (:noun result) 100) (:verb result))))


(defn- evaluate-s1 [input]
  (calculate-simple-result input 12 2))

(defn- evaluate-s2 [input]
  (calculate-complex-result input 19690720))

(aoc-validation/validate-result-day2!
  (evaluate-s1 day-2-input)
  (evaluate-s2 day-2-input))

