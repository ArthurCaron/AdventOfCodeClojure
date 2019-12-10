(ns exercises.adventofcode.d3
  (:require [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]))

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

(defn calc-points [direction previous-result -]
  (let [partial-calc-new-point (partial calc-new-point (move direction))]
    {:last   (partial-calc-new-point previous-result)
     :result (calc-new-result previous-result)}))

(defn points-from-move-action [previous-result {direction :direction, amount :amount}]
  (reduce
    (partial calc-points direction)
    previous-result
    (range amount)))

(defn key-points-from-move-action [wire]
  (as-> (reduce points-from-move-action {:last [0 0] :result []} wire) it
        (calc-new-result it)
        (rest it)))

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
    manhattan-intersections)
  (reduce
    (fn [val1 val2] (if (< (:manhattan val1) (:manhattan val2)) val1 val2))
    manhattan-intersections))


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


(defn file->wires [file]
  (aoc-io/day-3-input-from-file! file))

(defn- evaluate-s1 [file]
  (as-> file it
        (file->wires it)
        (map key-points-from-move-action it)
        (map set it)
        (get-intersections it)
        (intersection-to-manhattan it)
        (find-lowest-distance it)
        (:manhattan it)))

(defn- evaluate-s2 [file]
  (let [wires (file->wires file)
        key-points (map key-points-from-move-action wires)
        intersections (get-intersections (map set key-points))
        partial-wires-distance-to-intersection (partial wires-distance-to-intersection key-points)]
    (as-> (map partial-wires-distance-to-intersection intersections) it
          (find-lowest-distance it)
          (:manhattan it))))


(aoc-validation/validate-result-day3!
  (evaluate-s1 (aoc-io/day-file 3))
  (evaluate-s2 (aoc-io/day-file 3)))
