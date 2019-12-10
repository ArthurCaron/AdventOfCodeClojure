(ns exercises.adventofcode.d5
  (:require [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]))

(defn partial-index->val [memory instruction-pointer]
  (fn [index] (nth memory (+ instruction-pointer index))))

(defn partial-index->index->val [memory instruction-pointer]
  (fn [index] (nth memory ((partial-index->val memory instruction-pointer) index))))

(defn partial-assoc-val [memory]
  (fn [position value] (assoc memory position value)))

(defn partial-all [memory instruction-pointer]
  [(partial-index->val memory instruction-pointer)
   (partial-index->index->val memory instruction-pointer)
   (partial-assoc-val memory)])


(defn standard [fun [index->val index->index->val assoc-val]] "Standard operation"
  (assoc-val (index->val 3) (fun (index->index->val 1) (index->index->val 2))))

(defn input [input-val [index->val _ assoc-val]] "Input operation"
  (assoc-val (index->val 1) input-val))

(defn output [[_ index->index->val _]] "Output operation"
  (index->index->val 1))


(defn op-code-1 [memory instruction-pointer]
  (standard + (partial-all (:memory memory) instruction-pointer)))

(defn op-code-2 [memory instruction-pointer]
  (standard * (partial-all (:memory memory) instruction-pointer)))

(defn op-code-3 [memory instruction-pointer]
  (input (first (:inputs memory)) (partial-all (:memory memory) instruction-pointer)))

(defn op-code-4 [memory instruction-pointer]
  (output (partial-all (:memory memory) instruction-pointer)))


(defn evaluate
  ([memory] "Init with instruction-pointer at the start of memory"
   (evaluate memory 0))
  ([memory instruction-pointer] "Finds op-code and iterates or stops"
   (let [op-code (nth (:memory memory) instruction-pointer)]
     (cond
       (= op-code 99) memory
       (= op-code 1) (recur (op-code-1 memory instruction-pointer) (+ instruction-pointer 4))
       (= op-code 2) (recur (op-code-2 memory instruction-pointer) (+ instruction-pointer 4))
       (= op-code 3) (recur (op-code-3 memory instruction-pointer) (+ instruction-pointer 2))
       (= op-code 4) (recur (op-code-4 memory instruction-pointer) (+ instruction-pointer 2))
       ))))




(defn simple-test []
  (as-> {:inputs [5] :memory [1 1 1 0 99]} it
        (evaluate it)))


(defn calculate-simple-result [file input-val] "Star 1"
  (as-> file it
        (aoc-io/day-2-input-from-file! it)
        (hash-map :inputs [input-val] :memory it)
        (evaluate it)))


(defn- evaluate-s1 [file]
  (calculate-simple-result file 1))

(aoc-validation/validate-result-day5!
  (evaluate-s1 (aoc-io/day-file 2))
  (evaluate-s1 (aoc-io/day-file 2)))

