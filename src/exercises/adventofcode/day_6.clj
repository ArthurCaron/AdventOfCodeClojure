(ns exercises.adventofcode.day-6
  (:require [clojure.string :as str]
            [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]))

(defn day-6-input-from-file! [file-name]
  (->> (aoc-io/slurp-file! file-name)
       (str/trim-newline)
       (aoc-io/split-by-line-return)
       (map aoc-io/split-by-parenthesis)
       (reduce
         (fn [result [orbited orbiter]]
           (assoc result orbiter orbited))
         {})
       ))

(def day-6-input (day-6-input-from-file! (aoc-io/day-file 6)))


(defn get-orbited [orbit-relationships orbiter] (orbit-relationships orbiter))




(def test-input "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L")

(defn get-test-input []
  (->> test-input
       (str/trim-newline)
       (aoc-io/split-by-line-return)
       (map aoc-io/split-by-parenthesis)
       (reduce
         (fn [result [orbited orbiter]]
           (assoc result orbiter orbited))
         {})))






(defn nb-orbits
  ([get-orbited-fn orbiter]
   (nb-orbits get-orbited-fn orbiter 0))
  ([get-orbited-fn orbiter count]
   (let [orbited (get-orbited-fn orbiter)]
     (if (= orbited "COM")
       (inc count)
       (recur get-orbited-fn orbited (inc count))))))

(def memoized-nb-orbits (memoize nb-orbits))


(defn evaluate-memoized [orbit-relationships]
  (let [get-orbited-fn (partial get-orbited orbit-relationships)
        calculate-nb-orbits (partial memoized-nb-orbits get-orbited-fn)]
    (->> (keys orbit-relationships)
         (map calculate-nb-orbits)
         (reduce +))))

(defn evaluate [orbit-relationships]
  (let [get-orbited-fn (partial get-orbited orbit-relationships)
        calculate-nb-orbits (partial nb-orbits get-orbited-fn)]
    (->> (keys orbit-relationships)
         (map calculate-nb-orbits)
         (reduce +))))



(def test-eval (evaluate (get-test-input)))




(defn evaluate-s1 [orbit-relationships]
  (evaluate orbit-relationships))

(defn evaluate-s2 [orbit-relationships]
  (evaluate orbit-relationships))

(aoc-validation/validate-result-day6!
  (evaluate-s1 day-6-input)
  (evaluate-s2 day-6-input))
