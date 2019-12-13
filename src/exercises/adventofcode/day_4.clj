(ns exercises.adventofcode.day-4
  (:require [clojure.string :as str]
            [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]
            [exercises.adventofcode.utils :as aoc-utils]))

(defn day-4-input-from-file! [file-name]
  (as-> file-name it
        (aoc-io/slurp-file! it)
        (str/trim-newline it)
        (aoc-io/split-by-dash it)
        (map aoc-io/cast-str-to-int it)
        (into [] it)))

(def day-4-input (day-4-input-from-file! (aoc-io/day-file 4)))


(defn numbers->password-range [[password-min password-max]]
  (range password-min password-max))

(defn- filter-never-decrease-digits [digits]
  (reduce
    (fn [digit1 digit2]
      (if (> digit1 digit2)
        (reduced false)
        digit2))
    digits))

(defn filter-never-decrease [numbers]
  (filter (partial filter-never-decrease-digits) numbers))


(defn filter-adjacent-identical-digits [expected-count digits]
  (loop [[current-digit & remaining] digits
         count 1]
    (if (empty? remaining)
      false
      (let [new-count (if (= current-digit (first remaining))
                        (inc count)
                        1)]
        (if (= new-count expected-count)
          true
          (recur remaining new-count))))))

(defn filter-adjacent-identical [expected-count numbers]
  (filter (partial filter-adjacent-identical-digits expected-count) numbers))

(defn filter-adjacent-identical-digits-only-two [digits]
  (loop [[current-digit & remaining] digits
         count 1]
    (if (empty? remaining)
      (if (= count 2)
        true
        false)
      (let [new-count (if (= current-digit (first remaining))
                        (inc count)
                        1)]
        (if (and (= count 2) (= new-count 1))
          true
          (recur remaining new-count))))))

(defn filter-adjacent-identical-only-two [numbers]
  (filter (partial filter-adjacent-identical-digits-only-two) numbers))


(defn evaluate-s1 [input]
  (as-> input it
        (numbers->password-range it)
        (map aoc-utils/number->digits it)
        (filter-never-decrease it)
        (filter-adjacent-identical 2 it)
        (count it)))

(defn- evaluate-s2 [input]
  (as-> input it
        (numbers->password-range it)
        (map aoc-utils/number->digits it)
        (filter-never-decrease it)
        (filter-adjacent-identical-only-two it)
        (count it)))

(aoc-validation/validate-result-day4!
  (evaluate-s1 day-4-input)
  (evaluate-s2 day-4-input))
