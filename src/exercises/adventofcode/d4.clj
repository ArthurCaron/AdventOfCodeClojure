(ns exercises.adventofcode.d4
  (:require [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]
            [clojure.math.numeric-tower :as math]))


(defn file->numbers [file]
  (aoc-io/day-4-input-from-file file))

(defn numbers->password-range [[password-min password-max]]
  (range password-min password-max))

(defn number->digits
  ([number]
   (number->digits '() number))
  ([result-seq number]
   (if (<= number 0)
     result-seq
     (recur
       (conj result-seq (rem number 10))
       (quot number 10)))))

(defn- filter-never-decrease-digits [digits]
  (reduce
    (fn [digit1 digit2]
      (if (> digit1 digit2)
        (reduced false)
        digit2))
    digits))

(defn filter-never-decrease [numbers]
  (filter (partial filter-never-decrease-digits) numbers)
  )



(defn- filter-two-adjacent-digits-identical-digits [digits]
  (not
    (reduce
    (fn [digit1 digit2]
      (if (= digit1 digit2)
        (reduced false)
        digit2))
    digits)))

(defn filter-two-adjacent-digits-identical [numbers]
  (filter (partial filter-two-adjacent-digits-identical-digits) numbers)
  )


(defn filter-two-adjacent-digits-identical [numbers]
  (filter (fn [digits]
            (let [frequencies-digits (frequencies digits)
                  vals-freq (vals frequencies)]
              (println frequencies-digits)
              (println vals-freq)

              )

            ) numbers)
  )



(defn digits->string [digits]
  (apply str digits))

(defn evaluate-test [min max]
  (as-> [min (inc max)] it
        (numbers->password-range it)
        (map number->digits it)
        (filter-never-decrease it)
        (filter-two-adjacent-digits-identical it)
        (map digits->string it)))

(def test1
  (and
    (= (first (evaluate-test 111111 111111)) "111111")
    (= (first (evaluate-test 223450 223450)) nil)
    (= (first (evaluate-test 123789 123789)) nil)
    ))

(defn evaluate-s1 [file]
  (as-> file it
        (file->numbers it)
        (numbers->password-range it)
        (map number->digits it)
        (filter-never-decrease it)
        (filter-two-adjacent-digits-identical it)
        (count it)))

(defn- evaluate-s2 [file])

(aoc-validation/validate-result-day4
  (evaluate-s1 (aoc-io/day-file 4))
  (evaluate-s2 (aoc-io/day-file 4)))
