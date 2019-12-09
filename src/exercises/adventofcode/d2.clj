(ns exercises.adventofcode.d2
  (:require [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]))


(defn standard [fun memory instruction-pointer] "Standard operation"
  (let [first-value-index (nth memory (+ instruction-pointer 1))
        second-value-index (nth memory (+ instruction-pointer 2))
        final-position (nth memory (+ instruction-pointer 3))
        value (fun (nth memory first-value-index) (nth memory second-value-index))]
    (assoc memory final-position value)))


(defn get-instruction [op-code] "Define existing instructions"
  (get
    {1 #(standard + %1 %2)
     2 #(standard * %1 %2)}
    op-code))


(defn evaluate
  ([memory] "Init with instruction-pointer at the start of memory"
   (evaluate memory 0))
  ([memory instruction-pointer] "Finds op-code and iterates or stops"
   (let [op-code (nth memory instruction-pointer)]
     (if (= op-code 99)
       memory
       (recur ((get-instruction op-code) memory instruction-pointer) (+ instruction-pointer 4))))))


(defn evaluate-memory [memory noun verb] "Sets noun and verb then evaluates"
  (as-> memory it
        (assoc it 1 noun)
        (assoc it 2 verb)
        (evaluate it)))


(defn calculate-simple-result [file noun verb] "Star 1"
  (as-> file it
        (aoc-io/day-2-input-from-file! it)
        (evaluate-memory it noun verb)
        (nth it 0)))

(defn found-expected? [memory expected]
  (= (nth memory 0) expected))

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

