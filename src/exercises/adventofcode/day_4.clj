(ns exercises.adventofcode.day-4
  (:require [clojure.string :as str]
            [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]
            [exercises.adventofcode.utils :as aoc-utils]))

(defn input-from-file! [file-name]
  (->> (aoc-io/slurp-file! file-name)
       (str/trim-newline)
       (aoc-io/split-by-dash)
       (map aoc-io/cast-str-to-int)
       (into [])))

(def input (input-from-file! (aoc-io/day-file 4)))


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
  (->> (numbers->password-range input)
       (map aoc-utils/number->digits)
       (filter-never-decrease)
       (filter-adjacent-identical 2)
       (count)))

(defn- evaluate-s2 [input]
  (->> (numbers->password-range input)
       (map aoc-utils/number->digits)
       (filter-never-decrease)
       (filter-adjacent-identical-only-two)
       (count)))


(aoc-validation/validate-result :4 :s1 (evaluate-s1 input))
(aoc-validation/validate-result :4 :s2 (evaluate-s2 input))