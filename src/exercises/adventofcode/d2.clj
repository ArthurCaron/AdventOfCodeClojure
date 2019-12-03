(ns exercises.adventofcode.d2
  (:require [clojure.string :as str]))

(def d2_file "src/exercises/adventofcode/d2.txt")

(defn parse_str_to_int [str]
  (Integer/parseInt (re-find #"\A-?\d+" str)))

(defn read_file [file_name]
  (-> file_name ,,,
      (slurp ,,,)
      (str/split ,,, #",")))

(defn file_to_int_vec [file_name]
  (->> file_name
       (read_file ,,,)
       (map parse_str_to_int ,,,)
       (into [] ,,,)))


(defn standard [fun memory instruction_pointer] "Standard operation"
  (let [first_value_index  (nth memory (+ instruction_pointer 1))
        second_value_index (nth memory (+ instruction_pointer 2))
        final_position     (nth memory (+ instruction_pointer 3))
        value (fun (nth memory first_value_index) (nth memory second_value_index))]
    (assoc memory final_position value)))


(defn get_instruction [op_code] "Define existing instructions"
  (get
    {1 #(standard + %1 %2)
     2 #(standard * %1 %2)}
    op_code))


(defn evaluate
  ([memory] "Init with instruction_pointer at the start of memory"
   (evaluate memory 0))
  ([memory instruction_pointer] "Finds op_code and iterates or stops"
   (let [op_code (nth memory instruction_pointer)]
     (if (= op_code 99)
       memory
       (recur ((get_instruction op_code) memory instruction_pointer) (+ instruction_pointer 4))))))


(defn evaluate_memory [memory noun verb] "Sets noun and verb then evaluates"
  (-> memory
      (assoc ,,, 1 noun)
      (assoc ,,, 2 verb)
      (evaluate ,,,)))


(defn evaluate_from_file [file noun verb] "Star 1"
  (as-> (file_to_int_vec file) memory
        (evaluate_memory memory noun verb)
        (nth memory 0)))

(defn found_expected? [memory expected]
  (= (nth memory 0) expected))

(defn is_max? [value]
  (= value 99))

(defn evaluate_from_file_2 [file expected] "Star 2"
  (let [memory (file_to_int_vec file)]
    (loop [noun 0, verb 0]
      (as-> memory current_memory
            (evaluate_memory current_memory noun verb)
            (if (found_expected? current_memory expected)
              (reduced {:noun noun, :verb verb})
              (if (is_max? verb)
                (if (is_max? noun)
                  (println "NOT FOUND")
                  (recur (inc noun) 0))
                (recur noun (inc verb))))))))


(def d2s1_result (evaluate_from_file d2_file 12 2))
(def d2s2_result
  (let [result (deref (evaluate_from_file_2 d2_file 19690720))]
    (+ (* (:noun result) 100) (:verb result))))

(def validate_result
  (if (and
        (= d2s1_result 4462686)
        (= d2s2_result 5936))
    (println "SUCCESS - CODE WORKS")
    (println "FAILURE - CODE DOESN'T WORK")))

