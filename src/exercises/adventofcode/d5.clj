(ns exercises.adventofcode.d5
  (:require [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]
            [exercises.adventofcode.utils :as aoc-utils]))


(defn index->val [{memory :memory, instruction-pointer :instruction-pointer} index]
  (nth memory (+ instruction-pointer index)))

(defn index->index->val [{memory :memory, :as memory-map} index]
  (nth memory (index->val memory-map index)))

(defn op-mode->fn [mode]
  (if (= mode \0)
    index->index->val
    index->val))

(defn op-mode->fn-for-index [mode]
  (if (= mode \0)
    index->val
    index->index->val))

(defn op-modes->fn [modes param-index]
  (op-mode->fn (nth modes param-index)))

(defn op-modes->fn-for-index [modes param-index]
  (op-mode->fn-for-index (nth modes param-index)))

(defn assoc-val [memory-map position value]
  (assoc-in memory-map [:memory position] value))

(defn standard [fun memory-map op-code-modes] "Standard operation"
  (assoc-val memory-map
             ((op-modes->fn-for-index op-code-modes 2) memory-map 3) ; Cheating here, I don't understand how it's supposed to be in position mode
             (fun
               ((op-modes->fn op-code-modes 0) memory-map 1)
               ((op-modes->fn op-code-modes 1) memory-map 2))))

(defn input [{input :input, :as memory-map} op-code-modes]
  (assoc-val memory-map ((op-modes->fn-for-index op-code-modes 0) memory-map 1) input))

(defn output [{:as memory-map} op-code-modes]
  (let [output-value ((op-modes->fn op-code-modes 0) memory-map 1)]
    (update-in memory-map [:outputs] #(conj % output-value))))


(defn jump [amount {instruction-pointer :instruction-pointer, :as memory-map}]
  (assoc-in memory-map [:instruction-pointer] (+ instruction-pointer amount)))


(defn op-code-1 [memory-map op-code-modes] (jump 4 (standard + memory-map op-code-modes)))
(defn op-code-2 [memory-map op-code-modes] (jump 4 (standard * memory-map op-code-modes)))
(defn op-code-3 [memory-map op-code-modes] (jump 2 (input memory-map op-code-modes)))
(defn op-code-4 [memory-map op-code-modes] (jump 2 (output memory-map op-code-modes)))


(defn get-op-code [{memory :memory, instruction-pointer :instruction-pointer}]
  (nth memory instruction-pointer))

(defn get-to-expected-size [op-code expected-size]
  (loop [digits (aoc-utils/number->digits op-code)]
    (if (< (count digits) expected-size)
      (recur (conj digits 0))
      digits)))

(defn get-op-code-mode [op-code expected-size]
  (let [digits (get-to-expected-size op-code expected-size)]
    (apply str digits)))

(defn op-code-mode->op-code [op-code-str]
  (str (nth op-code-str 3) (nth op-code-str 4)))

(defn op-code-mode->op-mode [op-code expected-size]
  (loop [result []
         remaining (- expected-size 2)]
    (if (= 0 remaining)
      result
      (recur (conj result (nth op-code (dec remaining))) (dec remaining)))))


(defn evaluate
  ([memory-map] "Finds op-code and iterates or stops"
   (let [op-code-mode (get-op-code-mode (get-op-code memory-map) 5)
         op-code (op-code-mode->op-code op-code-mode)
         op-mode (op-code-mode->op-mode op-code-mode 5)]

     ;(println (str "222 " (nth (:memory memory-map) 222) " 225 " (nth (:memory memory-map) 225)))
     ;(println (str op-code-mode " " (:instruction-pointer memory-map) " " (:outputs memory-map) " " (:memory memory-map)))

     (cond
       ;(and (not= (first (:outputs memory-map)) 0) (not-empty (:outputs memory-map))) nil
       (= op-code "99") memory-map
       (= op-code "01") (recur (op-code-1 memory-map op-mode))
       (= op-code "02") (recur (op-code-2 memory-map op-mode))
       (= op-code "03") (recur (op-code-3 memory-map op-mode))
       (= op-code "04") (recur (op-code-4 memory-map op-mode))
       ;true (println (str "wrong op-code-mode " op-code-mode))
       ))))






(defn simple-test []
  ;(evaluate {:input 1 :memory [1003 1 1104 0 1099] :instruction-pointer 0})
  (evaluate {:input 1 :outputs [] :memory [1002, 4, 3, 4, 33] :instruction-pointer 0})
  )





(defn calculate-simple-result [file input-val] "Star 1"
  (as-> file it
        (aoc-io/day-5-input-from-file! it)
        (hash-map :input input-val :outputs [] :memory it :instruction-pointer 0)
        (evaluate it)))


(defn evaluate-s1 [file]
  (last (:outputs (calculate-simple-result file 1))))

(aoc-validation/validate-result-day5!
  (evaluate-s1 (aoc-io/day-file 5))
  (evaluate-s1 (aoc-io/day-file 5)))

