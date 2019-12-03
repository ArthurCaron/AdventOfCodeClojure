(ns exercises.adventofcode.d1
  (:gen-class))

(def d1_file "src/exercises/adventofcode/d1.txt")

(defn read_file [file_name]
  (with-open [rdr (clojure.java.io/reader file_name)]
    (reduce conj [] (line-seq rdr))))


(defn calculate_fuel [mass]
  (-> mass
      (/ ,,, 3)
      (Math/floor ,,,)
      (- ,,, 2)))

(defn calculate_fuel_recur [mass]
  (loop [total_amount 0, remaining_weight mass]
    (let [fuel_weight (calculate_fuel remaining_weight)]
      (if (<= fuel_weight 0)
        total_amount
        (recur (+ total_amount fuel_weight) fuel_weight)))))


(defn parse_string_to_int_and_apply [mass_as_string fn]
  (as-> mass_as_string mass
        (Integer/parseInt (re-find #"\A-?\d+" mass))
        (fn mass)))


(defn calculate_result_with_fn [file fn]
  (->> (read_file file)
       (map #(parse_string_to_int_and_apply %1 fn) ,,,)
       (reduce + ,,,)
       (int ,,,)))


(def d1s1_result (calculate_result_with_fn d1_file calculate_fuel))
(def d1s2_result (calculate_result_with_fn d1_file calculate_fuel_recur))

(def validate_result
  (if (and
        (= d1s1_result 3442987)
        (= d1s2_result 5161601))
    (println "SUCCESS - CODE WORKS")
    (println "FAILURE - CODE DOESN'T WORK")))
