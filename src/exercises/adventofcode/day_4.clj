(ns exercises.adventofcode.day-4
  (:require [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]
            [exercises.adventofcode.utils :as aoc-utils]))


(defn file->numbers [file]
  (aoc-io/day-4-input-from-file! file))

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


(defn evaluate-s1 [file]
  (as-> file it
        (file->numbers it)
        (numbers->password-range it)
        (map aoc-utils/number->digits it)
        (filter-never-decrease it)
        (filter-adjacent-identical 2 it)
        (count it)))

(defn- evaluate-s2 [file]
  (as-> file it
        (file->numbers it)
        (numbers->password-range it)
        (map aoc-utils/number->digits it)
        (filter-never-decrease it)
        (filter-adjacent-identical-only-two it)
        (count it)))

(aoc-validation/validate-result-day4!
  (evaluate-s1 (aoc-io/day-file 4))
  (evaluate-s2 (aoc-io/day-file 4)))
