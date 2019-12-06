(ns exercises.adventofcode.validation)

; Day 1
(defn validate-result-day1 [d1s1-result d1s2-result]
  (if (and
        (= d1s1-result 3442987)
        (= d1s2-result 5161601))
    (println "SUCCESS - CODE WORKS")
    (println "FAILURE - CODE DOESN'T WORK")))

; Day 2
(defn validate-result-day2 [d2s1-result d2s2-result]
  (if (and
        (= d2s1-result 4462686)
        (= d2s2-result 5936))
    (println "SUCCESS - CODE WORKS")
    (println "FAILURE - CODE DOESN'T WORK")))

; Day 3
(defn validate-result-day3 [d3s1-result d3s2-result]
  (if (and
        (= d3s1-result 2180)
        (= d3s2-result 0))
    (println "SUCCESS - CODE WORKS")
    (println "FAILURE - CODE DOESN'T WORK")))