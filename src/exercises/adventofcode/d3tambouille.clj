(ns exercises.adventofcode.d3tambouille
  (:require [exercises.adventofcode.io :as aoc-io]
            [exercises.adventofcode.validation :as aoc-validation]))

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
  (let [last (:last result)]
    (assoc last :distance (inc (:distance last)),
                :coords (move-action (:coords last)))))
;(move-action (:last result)))

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
  (as-> (reduce points-from-move-action {:last {:distance 0, :coords [0,0]} :result []} wire) result
        (calc-new-result result)
        (rest result)))

(defn get-intersections [wires]
  (let [key-points (map key-points-from-move-action wires)
        key-points-as-set (map (partial into #{}) (map (fn [points] (map #(:coords %) points)) key-points))]
    {:key-points key-points,
     :intersections (apply clojure.set/intersection key-points-as-set)}))

(defn intersections-to-manhattan [wires]
  (assoc wires :intersections
               (map
                 (fn [intersection]
                   {:intersection intersection,
                    :manhattan    (manhattan-distance intersection)})
                 (:intersections wires))))

(defn find-lowest-distance [manhattan-intersections]
  (if (= (count manhattan-intersections) 1)
    manhattan-intersections)
  (reduce
    (fn [val1 val2] (if (< (:manhattan val1) (:manhattan val2)) val1 val2))
    manhattan-intersections))

(defn evaluate-d3s1 [file]
  (as-> file it
        (aoc-io/day-3-input-from-file it)
        (get-intersections it)
        (intersections-to-manhattan it)
        (:intersections it)
        (find-lowest-distance it)
        (:manhattan it)
        ))

(defn evaluate-d3s2 [wires]
  (as-> wires it
        (get-intersections it)
        ;(intersections-to-manhattan it)
        ;(:intersections it)
        ;(find-lowest-distance it)
        ;(:manhattan it)
        ))



























(defn evaluate-from-file-2 [file]
  (-> file
      (aoc-io/day-3-input-from-file,,,)
      (evaluate-d3s2,,,)))

(evaluate-d3s2
  (aoc-io/day-3-input-from-code
    "R3,U3,L1,D1"
    "U2,R4"))

(def test1-eval
  (evaluate-d3s2
    (aoc-io/day-3-input-from-code
      "R75,D30,R83,U83,L12,D49,R71,U7,L72"
      "U62,R66,U55,R34,D71,R55,D58,R83")))
(def test2-eval
  (evaluate-d3s2
    (aoc-io/day-3-input-from-code
      "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
      "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")))

(def test1
  (if (= test1-eval 610)
    (println "SUCCESS - CODE WORKS")
    (println "FAILURE - CODE DOESN'T WORK")))

(def test2
  (if (= test2-eval 410)
    (println "SUCCESS - CODE WORKS")
    (println "FAILURE - CODE DOESN'T WORK")))


(def d3s1-result (evaluate-d3s1 (aoc-io/day-file 3)))
(def validate-result-day3
  (if (= d3s1-result 2180)
    (println "SUCCESS - CODE WORKS")
    (println "FAILURE - CODE DOESN'T WORK")))

(def d3s2-result (evaluate-from-file-2 (aoc-io/day-file 3)))

(aoc-validation/validate-result-day3 d3s1-result d3s2-result)
