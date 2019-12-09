(ns exercises.adventofcode.validation)

(defn validate! [result expected]
  (if (= result expected)
    (println "SUCCESS - CODE WORKS")
    (println "FAILURE - CODE DOESN'T WORK")))

; Day 1
(defn validate-result-day1! [s1-result s2-result]
  (validate! s1-result 3442987)
  (validate! s2-result 5161601))

; Day 2
(defn validate-result-day2! [s1-result s2-result]
  (validate! s1-result 4462686)
  (validate! s2-result 5936))

; Day 3
(defn validate-result-day3! [s1-result s2-result]
  (validate! s1-result 2180)
  (validate! s2-result 112316))

; Day 4
(defn validate-result-day4! [s1-result s2-result]
  (validate! s1-result 2150)
  (validate! s2-result 1462))

; Day 5
(defn validate-result-day5! [s1-result s2-result]
  (validate! s1-result 0)
  (validate! s2-result 0))