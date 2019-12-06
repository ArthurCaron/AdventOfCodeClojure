(ns exercises.exercise
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [clojure.spec.test.alpha :as st]))

(defn n-matrix
  "Returns a lazy-sequence representing a square matrix of `n` with
  increasing numbers between 0 and n^2 - 1.
​
  (n-matrix 1) => ((0))
​
  (n-matrix 2) => ((0 1)
                   (2 3))
​
  (n-matrix 3) => ((0 1 2)
                   (3 4 5)
                   (6 7 8))"
  [n]
  (map (fn [start_value] (range start_value (+ start_value n)))
       (range 0 (* n n) n)))

(defn n-matrix-2
  [n]
  (loop [i 0, result []]
    (if (= i n)
      result
      (let [start_value (* i n)]
        (recur (inc i) (into result (vector (range start_value (+ start_value n)))))))))

(defn n-matrix-3
  [n]
  (partition n (range (* n n))))


(defn sum-column
  "Returns a lazy-sequence of size (count matrix) where values are the sum of
  each column of the matrix.
​
  (sum-column '((0 1 2)
                (3 4 5)
                (6 7 8))) => (9 12 15)"
  [matrix]
  (apply map + matrix))

(defn average
  "Compute the average of the sequence using only reduce (no call to count)."
  [coll]
  (/ (reduce + coll) (reduce (fn [counter _] (inc counter)) 0 coll)))



(s/fdef rev
        :args (s/cat :s string?)
        :ret string?
        :fn (fn [{:keys [args ret]}]
              (= ret (-> args :s str/reverse))))

(defn rev
  "Same as `clojure.string/reverse`."
  [s]
  (str/join (reduce conj '() s)))

(defn rev-cool
  "Same as `clojure.string/reverse`."
  [s]
  (apply str (apply conj '() s)))

(deftest rev-test
  (is (= 1 (-> (st/check `rev)
               (st/summarize-results)
               :check-passed))))


(defn filter-anagram [lc-word word-frequencies val]
  (let [lc-val (str/lower-case val)
        val-frequencies (frequencies lc-val)]
    (and (not= lc-word lc-val) (= word-frequencies val-frequencies))))

(defn anagrams-for [word prospect-list]
  (let [lc-word (str/lower-case word)
        partial-filter (partial filter-anagram lc-word (frequencies lc-word))]
    (filter partial-filter prospect-list)))


(deftest no-matches
  (is (= []
         (anagrams-for "diaper" ["hello" "world" "zombies" "pants"]))))

(deftest detect-simple-anagram
  (is (= ["tan"] (anagrams-for "ant" ["tan" "stand" "at"]))))

(deftest does-not-confuse-different-duplicates
  (is (= [] (anagrams-for "galea" ["eagle"]))))

(deftest eliminate-anagram-subsets
  (is (= [] (anagrams-for "good" ["dog" "goody"]))))

(deftest detect-anagram
  (is (= ["inlets"]
         (let [coll ["enlists" "google" "inlets" "banana"]]
           (anagrams-for "listen" coll)))))

(deftest multiple-anagrams
  (is (= ["gallery" "regally" "largely"]
         (let [coll ["gallery" "ballerina" "regally"
                     "clergy" "largely" "leading"]]
           (anagrams-for "allergy" coll)))))

(deftest case-insensitive-anagrams
  (is (= ["Carthorse"]
         (let [coll ["cashregister" "Carthorse" "radishes"]]
           (anagrams-for "Orchestra" coll)))))

(deftest word-is-not-own-anagram
  (is (= [] (anagrams-for "banana" ["banana"]))))

(deftest capital-word-is-not-own-anagram
  (is (= [] (anagrams-for "BANANA" ["banana"]))))


(defn all-tests []
  (run-tests))