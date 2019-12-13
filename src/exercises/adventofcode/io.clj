(ns exercises.adventofcode.io
  (:require [clojure.string :as str]))

(defn day-file [day] (str "src/exercises/adventofcode/inputfiles/day-" day ".txt"))

(defn slurp-file! [file-name]
  (as-> file-name it
        (slurp it)))

(defn split-by-line-return [str]
  (str/split str #"\n"))

(defn split-by-comma [str]
  (str/split str #","))

(defn split-by-dash [str]
  (str/split str #"-"))

(defn parse-str-to-int [str]
  (Integer/parseInt (re-find #"\d+" str)))

(defn cast-str-to-int [str]
  (Integer/parseInt str))