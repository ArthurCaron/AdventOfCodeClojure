(ns exercises.adventofcode.io
  (:require [clojure.string :as str]))

(defn day-file [day] (str "src/exercises/adventofcode/inputfiles/day-" day ".txt"))

(defn- slurp-file! [file-name]
  (as-> file-name it
        (slurp it)))

(defn- split-by-line-return [str]
  (str/split str #"\n"))

(defn- split-by-comma [str]
  (str/split str #","))

(defn- split-by-dash [str]
  (str/split str #"-"))

(defn- parse-str-to-int [str]
  (Integer/parseInt (re-find #"\d+" str)))

(defn- cast-str-to-int [str]
  (Integer/parseInt str))


; Day 1
(defn day-1-input-from-file! [file-name]
  (as-> file-name it
        (slurp-file! it)
        (split-by-line-return it)
        (into [] it)))

; Day 2
(defn day-2-input-from-file! [file-name]
  (as-> file-name it
        (slurp-file! it)
        (str/trim-newline it)
        (split-by-comma it)
        (map cast-str-to-int it)
        (into [] it)))

; Day 3
(defn- parse [input]
  {:direction (first input)
   :amount    (parse-str-to-int input)})

(defn- parse-wire [wire]
  (map parse wire))

(defn day-3-input-from-file! [file-name]
  (as-> file-name it
        (slurp-file! it)
        (split-by-line-return it)
        (map split-by-comma it)
        (map parse-wire it)
        (into [] it)))

; Day 4
(defn day-4-input-from-file! [file-name]
  (as-> file-name it
        (slurp-file! it)
        (str/trim-newline it)
        (split-by-dash it)
        (map cast-str-to-int it)
        (into [] it)))

; Day 5
(defn day-5-input-from-file! [file-name]
  (as-> file-name it
        (slurp-file! it)
        (str/trim-newline it)
        (split-by-comma it)
        (map cast-str-to-int it)
        (into [] it)))
