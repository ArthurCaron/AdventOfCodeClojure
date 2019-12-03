(ns exercises.adventofcode.d3
  (:require [clojure.string :as str]))

(def d3_file "src/exercises/adventofcode/d3.txt")

(defn split_by_line_return [str]
  (str/split str #"\n"))

(defn split_by_comma [str]
  (str/split str #","))

(defn read_file [file_name]
  (map split_by_comma (split_by_line_return (slurp file_name))))

(defn get_amount_from_input [input]
  (->> input
       (re-find #"\d+",,,)
       (Integer/parseInt,,,)))

(defn parse [input]
  {:direction (first input)
   :amount    (get_amount_from_input input)})

(defn file_to_wires [file_name]
  (let [[first_wire second_wire] (read_file file_name)]
    {:first_wire  (map parse first_wire),
     :second_wire (map parse second_wire)}))


(defn abs [n] (max n (- n)))

(defn manhattan-distance [[x y]] (+ (abs x) (abs y)))


; TODO with list of points
; Also TODO with segments

(defn move [direction]
  (get
    {\U (fn [[x y]] [(inc x) y]),
     \R (fn [[x y]] [x (inc y)]),
     \D (fn [[x y]] [(dec x) y]),
     \L (fn [[x y]] [x (dec y)])}
    direction))

(defn calc-new-point [move-action result]
  (move-action (:last result)))

(defn calc-new-result [result]
  (conj (:result result) (:last result)))

(defn calc-points [direction previous-result _]
  (let [partial-calc-new-point (partial calc-new-point (move direction))]
    {:last   (partial-calc-new-point previous-result)
     :result (calc-new-result previous-result)}))

(defn points-from-move-action [previous-result {direction :direction, amount :amount}]
  (reduce
    (partial calc-points direction)
    previous-result
    (range amount)))

(defn key-points-from-move-action [wire]
  (as-> (reduce points-from-move-action {:last [0 0] :result []} wire) result
        (calc-new-result result)
        (rest result)
        (into #{} result)))

(defn get-intersections [wires]
  (->> wires
       (vals ,,,)
       (map key-points-from-move-action ,,,)
       (apply clojure.set/intersection ,,,)))

(defn intersection-to-manhattan [intersections]
  (map
    (fn [intersection]
      {:intersection intersection,
       :manhattan (manhattan-distance intersection)})
    intersections))

(defn find-lowest-distance [manhattan-intersections]
  (if (= (count manhattan-intersections) 1)
    manhattan-intersections)
  (reduce (fn [val1 val2] (if (< (:manhattan val1) (:manhattan val2)) val1 val2))  manhattan-intersections))



(defn evaluate [wires]
  (-> wires
      (get-intersections ,,,)
      (intersection-to-manhattan ,,,)
      (find-lowest-distance ,,,)))

(defn evaluate_from_file [file]
  (-> file
      (file_to_wires,,,)
      (evaluate,,,)))


(defn take_lists [str_list_1, str_list_2]
  {:first_wire  (as-> str_list_1 input
                      (str/split input #",")
                      (map parse input))
   :second_wire (as-> str_list_2 input
                      (str/split input #",")
                      (map parse input))})

(def test1_eval (take_lists "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83"))

(def test1
  (if (= (evaluate (take_lists "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83"))
         159)
    (println "SUCCESS - CODE WORKS")
    (println "FAILURE - CODE DOESN'T WORK")))

(def test2
  (if (= (evaluate (take_lists "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                               "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))
         135)
    (println "SUCCESS - CODE WORKS")
    (println "FAILURE - CODE DOESN'T WORK")))


(def d3s1_result (evaluate_from_file d3_file))
(def d3s2_result (evaluate_from_file d3_file))

(def validate_result
  (if (and
        (= d3s1_result 159)
        (= d3s2_result 159))
    (println "SUCCESS - CODE WORKS")
    (println "FAILURE - CODE DOESN'T WORK")))


(evaluate
  (take_lists
    "R3,D3,R8,U3,L1,D4,R1,U7,L2"
    "U2,R6,U5,R4,D1,R5,D8,R3"))