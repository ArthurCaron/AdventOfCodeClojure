(ns exercises.adventofcode.day-1
  (:require [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]))

(defn day-1-input-from-file! [file-name]
  (->> (aoc-io/slurp-file! file-name)
       (aoc-io/split-by-line-return)
       (into [])))

(def day-1-input (day-1-input-from-file! (aoc-io/day-file 1)))


(defn calculate-fuel [mass]
  (- (Math/floor (/ mass 3)) 2))

(defn calculate-fuel-recur [mass]
  (loop [total-amount 0, remaining-weight mass]
    (let [fuel-weight (calculate-fuel remaining-weight)]
      (if (<= fuel-weight 0)
        total-amount
        (recur (+ total-amount fuel-weight) fuel-weight)))))


(defn parse-string-to-int-and-apply [mass-as-string fn]
  (->> (re-find #"\A-?\d+" mass-as-string)
       (Integer/parseInt)
       (fn)))

(defn calculate-result-with-fn [input fn]
  (->> (map #(parse-string-to-int-and-apply %1 fn) input)
       (reduce +)
       (int)))


(defn evaluate-s1 [input]
  (calculate-result-with-fn input calculate-fuel))

(defn evaluate-s2 [input]
  (calculate-result-with-fn input calculate-fuel-recur))


(aoc-validation/validate-result-day1!
  (evaluate-s1 day-1-input)
  (evaluate-s2 day-1-input))
