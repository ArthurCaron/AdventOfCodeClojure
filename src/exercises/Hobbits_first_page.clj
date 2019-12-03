(ns exercises.Hobbits-first-page)

(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])

(defn my-reduce
  ([f initial coll]
   (loop [result initial
          remaining coll]
     (if (empty? remaining)
       result
       (recur (f result (first remaining)) (rest remaining)))))
  ([f [head & tail]]
   (my-reduce f head tail)))

(defn matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(defn better-symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (my-reduce (fn [final-body-parts part]
               (into final-body-parts (set [part (matching-part part)])))
             []
             asym-body-parts))

(defn add-100-to-number
  [number]
  (+ number 100))


(defn dec-maker
  [substract-value]
  (fn [value] (- value substract-value)))

(def dec9 (dec-maker 9))
(dec9 10)

(defn dec-maker-short
  [substract-value]
  #(- %1 substract-value))

(def dec9 (dec-maker-short 9))
(dec9 10)

(def dec9 #(- %1 9))
(dec9 10)

(defn mapset
  [f values]
  (into #{} (map f values)))

(mapset inc [1 1 2 2])
; => #{2 3}

; Create a function that’s similar to symmetrize-body-parts except that it has to work with weird space aliens with radial symmetry.
; Instead of two eyes, arms, legs, and so on, they have five.


; Create a function that generalizes symmetrize-body-parts and the function you created in Exercise 5.
; The new function should take a collection of body parts and the number of matching body parts to add.
; If you’re completely new to Lisp languages and functional programming, it probably won’t be obvious how to do this.
; If you get stuck, just move on to the next chapter and revisit the problem later.


(def asym-creature-body-parts [{:name "head" :size 3}
                               {:name "left-eye" :size 1}
                               {:name "left-ear" :size 1}
                               {:name "mouth" :size 1}
                               {:name "nose" :size 1}
                               {:name "neck" :size 2}
                               {:name "left-shoulder" :size 3}
                               {:name "left-upper-arm" :size 3}
                               {:name "chest" :size 10}
                               {:name "back" :size 10}
                               {:name "left-forearm" :size 3}
                               {:name "abdomen" :size 6}
                               {:name "left-kidney" :size 1}
                               {:name "left-hand" :size 2}
                               {:name "left-knee" :size 2}
                               {:name "left-thigh" :size 4}
                               {:name "left-lower-leg" :size 3}
                               {:name "left-achilles" :size 1}
                               {:name "left-foot" :size 2}])

(defn add-number-to-name
  ([part]
   #(add-number-to-name part %1))
  ([part number]
   { :name (str (:name part) "-" number) :size (:size part) }))

(defn do-fn-inc-number-of-times
  [f nb-times]
  (loop [number 1
         result []]
    (if (> number nb-times)
      result
      (recur (inc number)
             (into result (vector (f number)))))))

(def matcher #"^left-")

(defn differenciate-alien
  [part number-of-each]
  (if (re-find matcher (:name part))
    (into (do-fn-inc-number-of-times (add-number-to-name part) number-of-each)
          (do-fn-inc-number-of-times (add-number-to-name {:name (clojure.string/replace (:name part) #"^left-" "right-")
                                                          :size (:size part)}) number-of-each))
    (vector part))
  )

(defn generic-symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (differenciate-alien part 5)))
          []
          asym-body-parts))

(generic-symmetrize-body-parts asym-creature-body-parts)





