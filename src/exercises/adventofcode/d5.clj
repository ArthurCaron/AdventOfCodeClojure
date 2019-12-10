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

(defn jump [amount {instruction-pointer :instruction-pointer, :as memory-map}]
  (assoc-in memory-map [:instruction-pointer] (+ instruction-pointer amount)))


(defn standard [fun memory-map op-code-modes] "Standard operation"
  (assoc-val memory-map
             ((op-modes->fn-for-index op-code-modes 2) memory-map 3) ; Cheating here, I don't understand how it's supposed to be in position mode
             (fun
               ((op-modes->fn op-code-modes 0) memory-map 1)
               ((op-modes->fn op-code-modes 1) memory-map 2))))

(defn input [{input :input, :as memory-map} op-code-modes]
  (assoc-val memory-map ((op-modes->fn-for-index op-code-modes 0) memory-map 1) input))

(defn output [memory-map op-code-modes]
  (let [output-value ((op-modes->fn op-code-modes 0) memory-map 1)]
    (update-in memory-map [:outputs] #(conj % output-value))))

(defn jump-if-true [memory-map op-code-modes]
  (let [first-param ((op-modes->fn op-code-modes 0) memory-map 1)
        second-param ((op-modes->fn op-code-modes 1) memory-map 2)]
  (if (not= first-param 0)
    (assoc-in memory-map [:instruction-pointer] second-param)
    (jump 3 memory-map))))

(defn jump-if-false [memory-map op-code-modes]
  (let [first-param ((op-modes->fn op-code-modes 0) memory-map 1)
        second-param ((op-modes->fn op-code-modes 1) memory-map 2)]
    (if (= first-param 0)
      (assoc-in memory-map [:instruction-pointer] second-param)
      (jump 3 memory-map))))

(defn less-than [memory-map op-code-modes]
  (let [first-param ((op-modes->fn op-code-modes 0) memory-map 1)
        second-param ((op-modes->fn op-code-modes 1) memory-map 2)
        third-param ((op-modes->fn-for-index op-code-modes 2) memory-map 3)]
    (if (< first-param second-param)
      (assoc-val memory-map third-param 1)
      (assoc-val memory-map third-param 0))))

(defn equals [memory-map op-code-modes]
  (let [first-param ((op-modes->fn op-code-modes 0) memory-map 1)
        second-param ((op-modes->fn op-code-modes 1) memory-map 2)
        third-param ((op-modes->fn-for-index op-code-modes 2) memory-map 3)]
    (if (= first-param second-param)
      (assoc-val memory-map third-param 1)
      (assoc-val memory-map third-param 0))))


(defn op-code-1 [memory-map op-code-modes] (jump 4 (standard + memory-map op-code-modes)))
(defn op-code-2 [memory-map op-code-modes] (jump 4 (standard * memory-map op-code-modes)))
(defn op-code-3 [memory-map op-code-modes] (jump 2 (input memory-map op-code-modes)))
(defn op-code-4 [memory-map op-code-modes] (jump 2 (output memory-map op-code-modes)))
(defn op-code-5 [memory-map op-code-modes] (jump-if-true memory-map op-code-modes))
(defn op-code-6 [memory-map op-code-modes] (jump-if-false memory-map op-code-modes))
(defn op-code-7 [memory-map op-code-modes] (jump 4 (less-than memory-map op-code-modes)))
(defn op-code-8 [memory-map op-code-modes] (jump 4 (equals memory-map op-code-modes)))


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

     (cond
       (= op-code "99") memory-map
       (= op-code "01") (recur (op-code-1 memory-map op-mode))
       (= op-code "02") (recur (op-code-2 memory-map op-mode))
       (= op-code "03") (recur (op-code-3 memory-map op-mode))
       (= op-code "04") (recur (op-code-4 memory-map op-mode))
       (= op-code "05") (recur (op-code-5 memory-map op-mode))
       (= op-code "06") (recur (op-code-6 memory-map op-mode))
       (= op-code "07") (recur (op-code-7 memory-map op-mode))
       (= op-code "08") (recur (op-code-8 memory-map op-mode))
       true (println (str "wrong op-code-mode " op-code-mode))
       ))))






(defn simple-test []
  ;(evaluate {:input 1 :memory [1003 1 1104 0 1099] :instruction-pointer 0})
  ;(evaluate {:input 1 :outputs [] :memory [1002, 4, 3, 4, 33] :instruction-pointer 0})
  (evaluate {:input 8 :outputs [] :memory [3,9,8,9,10,9,4,9,99,-1,8] :instruction-pointer 0})
  (evaluate {:input 7 :outputs [] :memory [3,9,8,9,10,9,4,9,99,-1,8] :instruction-pointer 0})

  (evaluate {:input 7 :outputs [] :memory [3,9,7,9,10,9,4,9,99,-1,8] :instruction-pointer 0})
  (evaluate {:input 9 :outputs [] :memory [3,9,7,9,10,9,4,9,99,-1,8] :instruction-pointer 0})

  (evaluate {:input 8 :outputs [] :memory [3,3,1108,-1,8,3,4,3,99] :instruction-pointer 0})
  (evaluate {:input 7 :outputs [] :memory [3,3,1108,-1,8,3,4,3,99] :instruction-pointer 0})

  (evaluate {:input 7 :outputs [] :memory [3,3,1107,-1,8,3,4,3,99] :instruction-pointer 0})
  (evaluate {:input 9 :outputs [] :memory [3,3,1107,-1,8,3,4,3,99] :instruction-pointer 0})


  (evaluate {:input 0 :outputs [] :memory [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] :instruction-pointer 0})
  (evaluate {:input 5 :outputs [] :memory [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] :instruction-pointer 0})

  (evaluate {:input 0 :outputs [] :memory [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] :instruction-pointer 0})
  (evaluate {:input 5 :outputs [] :memory [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] :instruction-pointer 0})


  (evaluate {:input 7
             :outputs []
             :memory [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
             :instruction-pointer 0})
  (evaluate {:input 8
             :outputs []
             :memory [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
             :instruction-pointer 0})
  (evaluate {:input 9
             :outputs []
             :memory [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
             :instruction-pointer 0})
  )





(defn evaluate-result [file input-val] "Star 1"
  (as-> file it
        (aoc-io/day-5-input-from-file! it)
        (hash-map :input input-val :outputs [] :memory it :instruction-pointer 0)
        (evaluate it)))


(defn evaluate-s1 [file]
  (last (:outputs (evaluate-result file 1))))

(defn evaluate-s2 [file]
  (last (:outputs (evaluate-result file 5))))

(aoc-validation/validate-result-day5!
  (evaluate-s1 (aoc-io/day-file 5))
  (evaluate-s2 (aoc-io/day-file 5)))

