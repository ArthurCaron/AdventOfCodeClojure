(ns exercises.adventofcode.day-6
  (:require [clojure.string :as str]
            [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]))

(defn input-from-file! [file-name]
  (->> (aoc-io/slurp-file! file-name)
       (str/trim-newline)
       (aoc-io/split-by-line-return)
       (map aoc-io/split-by-parenthesis)
       (reduce
         (fn [result [orbited orbiter]]
           (assoc result orbiter orbited))
         {})
       ))

(def input (input-from-file! (aoc-io/day-file 6)))


(defn list-orbits
  [get-orbited-fn result orbiter]
   (let [orbited (get-orbited-fn orbiter)]
     (if (= orbited "COM")
       (conj result orbited)
       (recur get-orbited-fn (conj result orbited) orbited))))

(defn count-differences
  [coll1 coll2]
  (if (not= (first coll1) (first coll2))
    (+ (count coll1) (count coll2))
    (recur (rest coll1) (rest coll2)))
  )


(defn evaluate-s1 [orbit-relationships]
  (let [get-orbited-fn (partial get orbit-relationships)
        compute-list-orbits (partial list-orbits get-orbited-fn [])]
    (->> (keys orbit-relationships)
         (map compute-list-orbits)
         (map count)
         (reduce +))))

(defn evaluate-s2 [orbit-relationships]
  (let [get-orbited-fn (partial get orbit-relationships)
        compute-list-orbits (partial list-orbits get-orbited-fn [])]
    (->> (keys orbit-relationships)
         (filter #{"YOU" "SAN"})
         (map compute-list-orbits)
         (map reverse)
         (apply count-differences))))


(aoc-validation/validate-result :6 :s1 (evaluate-s1 input))
(aoc-validation/validate-result :6 :s2 (evaluate-s2 input))