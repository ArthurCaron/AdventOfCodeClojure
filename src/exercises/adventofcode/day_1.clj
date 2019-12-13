(ns exercises.adventofcode.day-1
  (:require [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]))

(defn day-1-input-from-file! [file-name]
  (as-> file-name it
        (aoc-io/slurp-file! it)
        (aoc-io/split-by-line-return it)
        (into [] it)))

(def day-1-input (day-1-input-from-file! (aoc-io/day-file 1)))


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

(defn calculate-result-with-fn [input fn]
  (as-> input it
        (map #(parse-string-to-int-and-apply %1 fn) it)
        (reduce + it)
        (int it)))


(defn- evaluate-s1 [input]
  (calculate-result-with-fn input calculate-fuel))

(defn- evaluate-s2 [input]
  (calculate-result-with-fn input calculate-fuel-recur))


(aoc-validation/validate-result-day1!
  (evaluate-s1 day-1-input)
  (evaluate-s2 day-1-input))
