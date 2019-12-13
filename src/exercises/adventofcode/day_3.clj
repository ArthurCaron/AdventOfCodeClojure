(ns exercises.adventofcode.day-3
  (:require [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]))

(defn parse [input]
  {:direction (first input)
   :amount    (aoc-io/parse-str-to-int input)})

(defn parse-wire [wire]
  (map parse wire))

(defn day-3-input-from-file! [file-name]
  (->> (aoc-io/slurp-file! file-name)
       (aoc-io/split-by-line-return)
       (map aoc-io/split-by-comma)
       (map parse-wire)
       (into [])))

(def day-3-input (day-3-input-from-file! (aoc-io/day-file 3)))


(defn abs [n] (max n (- n)))
(defn manhattan-distance [[x y]] (+ (abs x) (abs y)))

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
  (->> (reduce points-from-move-action {:last [0 0] :result []} wire)
       (calc-new-result)
       (rest)))

(defn get-intersections [wires]
  (apply clojure.set/intersection wires))

(defn intersection-to-manhattan [intersections]
  (map
    (fn [intersection]
      {:intersection intersection,
       :manhattan    (manhattan-distance intersection)})
    intersections))

(defn find-lowest-distance [manhattan-intersections]
  (if (= (count manhattan-intersections) 1)
    manhattan-intersections
    (reduce
      (fn [val1 val2] (if (< (:manhattan val1) (:manhattan val2)) val1 val2))
      manhattan-intersections)))


(defn wire-distance-to-intersection [wire intersection]
  (reduce
    (fn [distance val1]
      (if (= val1 intersection)
        (reduced (inc distance))
        (inc distance)))
    0
    wire))

(defn wires-distance-to-intersection [wires intersection]
  (reduce
    (fn [distance-result wire]
      (assoc
        distance-result
        :manhattan
        (+
          (:manhattan distance-result)
          (wire-distance-to-intersection wire intersection))))
    {:intersection intersection,
     :manhattan    0}
    wires))


(defn- evaluate-s1 [wires]
  (->> (map key-points-from-move-action wires)
       (map set)
       (get-intersections)
       (intersection-to-manhattan)
       (find-lowest-distance)
       (:manhattan)))

(defn- evaluate-s2 [wires]
  (let [key-points (map key-points-from-move-action wires)
        intersections (get-intersections (map set key-points))
        partial-wires-distance-to-intersection (partial wires-distance-to-intersection key-points)]
    (->> (map partial-wires-distance-to-intersection intersections)
         (find-lowest-distance)
         (:manhattan))))


(aoc-validation/validate-result-day3!
  (evaluate-s1 day-3-input)
  (evaluate-s2 day-3-input))
