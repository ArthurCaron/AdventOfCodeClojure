(ns exercises.adventofcode.day-1
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
        (aoc-io/day-1-input-from-file! it)
        (map #(parse-string-to-int-and-apply %1 fn) it)
        (reduce + it)
        (int it)))


(defn- evaluate-s1 [file]
  (calculate-result-with-fn file calculate-fuel))

(defn- evaluate-s2 [file]
  (calculate-result-with-fn file calculate-fuel-recur))


(aoc-validation/validate-result-day1!
  (evaluate-s1 (aoc-io/day-file 1))
  (evaluate-s2 (aoc-io/day-file 1)))
