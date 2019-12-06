(ns exercises.adventofcode.d1
  (:require [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]))


(defn calculate-fuel [mass]
  (as-> mass it
        (/ it 3)
        (Math/floor it)
        (- it 2)))

(defn calculate-fuel-recur [mass]
  (loop [total-amount 0, remaining-weight mass]
    (let [fuel-weight (calculate-fuel remaining-weight)]
      (if (<= fuel-weight 0)
        total-amount
        (recur (+ total-amount fuel-weight) fuel-weight)))))


(defn parse-string-to-int-and-apply [mass-as-string fn]
  (as-> mass-as-string it
        (Integer/parseInt (re-find #"\A-?\d+" it))
        (fn it)))

(defn calculate-result-with-fn [file fn]
  (as-> file it
        (aoc-io/day-1-input-from-file it)
        (map #(parse-string-to-int-and-apply %1 fn) it)
        (reduce + it)
        (int it)))

(defn evaluate-d1s1 []
  (calculate-result-with-fn (aoc-io/day-file 1) calculate-fuel))
(defn evaluate-d1s2 []
  (calculate-result-with-fn (aoc-io/day-file 1) calculate-fuel-recur))


(aoc-validation/validate-result-day1 (evaluate-d1s1) (evaluate-d1s2))
