(ns exercises.adventofcode.d2
  (:require [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]))

(defn index->val [{memory :memory instruction-pointer :instruction-pointer} index]
  (nth memory (+ instruction-pointer index)))

(defn index->index->val [{memory :memory :as memory-map} index]
  (nth memory (index->val memory-map index)))

(defn assoc-val [{memory :memory} position value]
  (assoc memory position value))

(defn assoc-val- [memory position value]
  (assoc-in memory [:memory position] value))

(defn standard- [fun memory-map] "Standard operation"
  (assoc-val- memory-map
             (index->val memory-map 3)
             (fun
               (index->index->val memory-map 1)
               (index->index->val memory-map 2))))

(defn standard [fun memory-map] "Standard operation"
  (assoc-val memory-map
             (index->val memory-map 3)
             (fun
               (index->index->val memory-map 1)
               (index->index->val memory-map 2))))


(defn op-code-1 [memory-map]
  (standard + memory-map))
(defn op-code-1- [memory-map]
  (standard- + memory-map))

(defn op-code-2 [memory-map]
  (standard * memory-map))


(defn get-op-code [{memory :memory instruction-pointer :instruction-pointer}]
  (nth memory instruction-pointer))

(defn jump [{instruction-pointer :instruction-pointer} amount]
  (+ instruction-pointer amount))

(defn jump- [{instruction-pointer :instruction-pointer :as memory-map} amount]
  (assoc-in memory-map [:instruction-pointer] (+ instruction-pointer amount)))
;(+ instruction-pointer amount))


(defn evaluate
  ([memory-map] "Finds op-code and iterates or stops"
   (let [op-code (get-op-code memory-map)]
     (cond
       (= op-code 99) memory-map
       (= op-code 1) (recur {:memory (op-code-1 memory-map)
                             :instruction-pointer (jump memory-map 4)})
       (= op-code 2) (recur {:memory (op-code-2 memory-map)
                             :instruction-pointer (jump memory-map 4)})
       ))))

(defn evaluate-memory [memory noun verb] "Sets noun and verb then evaluates"
  (as-> {:memory memory} it
        (assoc-in it [:memory 1] noun)
        (assoc-in it [:memory 2] verb)
        (:memory it)
        (evaluate {:memory it :instruction-pointer 0})))


(defn calculate-simple-result [file noun verb] "Star 1"
  (let [memory (aoc-io/day-2-input-from-file! file)]
    (as-> (evaluate-memory memory noun verb) it
          (nth (:memory it) 0))))

(defn found-expected? [memory expected]
  (= (nth (:memory memory) 0) expected))

(defn is-max? [value]
  (= value 99))

(defn loop-on-noun-and-verb [file expected]
  (let [memory (aoc-io/day-2-input-from-file! file)]
    (loop [noun 0, verb 0]
      (as-> memory current-memory
            (evaluate-memory current-memory noun verb)
            (if (found-expected? current-memory expected)
              (reduced {:noun noun, :verb verb})
              (if (is-max? verb)
                (if (is-max? noun)
                  (println "NOT FOUND")
                  (recur (inc noun) 0))
                (recur noun (inc verb))))))))

(defn calculate-complex-result [file expected] "Star 2"
  (let [result (deref (loop-on-noun-and-verb file expected))]
    (+ (* (:noun result) 100) (:verb result))))


(defn- evaluate-s1 [file]
  (calculate-simple-result file 12 2))

(defn- evaluate-s2 [file]
  (calculate-complex-result file 19690720))

(aoc-validation/validate-result-day2!
  (evaluate-s1 (aoc-io/day-file 2))
  (evaluate-s2 (aoc-io/day-file 2)))

