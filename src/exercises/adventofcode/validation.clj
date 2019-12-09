(ns exercises.adventofcode.validation)

(defn validate [s1-result s1-expected s2-result s2-expected]
  (if (and
        (= s1-result s1-expected)
        (= s2-result s2-expected))
    (println "SUCCESS - CODE WORKS")
    (println "FAILURE - CODE DOESN'T WORK")))

; Day 1
(defn validate-result-day1 [s1-result s2-result]
  (validate s1-result 3442987 s2-result 5161601))

; Day 2
(defn validate-result-day2 [s1-result s2-result]
  (validate s1-result 4462686 s2-result 5936))

; Day 3
(defn validate-result-day3 [s1-result s2-result]
  (validate s1-result 2180 s2-result 112316))

; Day 4
(defn validate-result-day4 [s1-result s2-result]
  (validate s1-result 0 s2-result 0))