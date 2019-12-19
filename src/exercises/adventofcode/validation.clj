(ns exercises.adventofcode.validation)


(def expected-results
  {:1  {:s1 3442987 :s2 5161601}
   :2  {:s1 4462686 :s2 5936}
   :3  {:s1 2180 :s2 112316}
   :4  {:s1 2150 :s2 1462}
   :5  {:s1 11933517 :s2 10428568}
   :6  {:s1 139597 :s2 286}
   :7  {:s1 567045 :s2 000}
   :8  {:s1 000 :s2 000}
   :9  {:s1 000 :s2 000}
   :10 {:s1 000 :s2 000}})


(defn validate! [result expected]
  (if (= result expected)
    (println "SUCCESS - CODE WORKS")
    (println "FAILURE - CODE DOESN'T WORK")))

(defn validate-result [day-key star-key result]
  (validate! result (star-key (day-key expected-results))))
