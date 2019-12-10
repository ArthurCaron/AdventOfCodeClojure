(ns exercises.adventofcode.utils)

(defn number->digits
  ([number]
   (number->digits '() number))
  ([result-seq number]
   (if (<= number 0)
     result-seq
     (recur
       (conj result-seq (rem number 10))
       (quot number 10)))))
