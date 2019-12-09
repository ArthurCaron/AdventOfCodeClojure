(ns exercises.adventofcode.part-4-glitter-filter)

(def filename "src/exercises/suspects.csv")

(defn validate [record]
  (and (contains? record :name) (contains? record :glitter-index)))

(defn append [record]
  (if (validate record)
    (spit filename (str (:name record) "," (:glitter-index record)) :append true)))

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name          identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(defn glitter-filter
  [minimum-glitter records]
  (map
    (fn [record] (:name record))
    (filter #(>= (:glitter-index %) minimum-glitter) records)))


(defn all [minimum-glitter]
  (glitter-filter minimum-glitter (mapify (parse (slurp filename)))))

(defn map-to-csv [records]
  (let [join-comma (partial clojure.string/join ",")
        join-new-line (partial clojure.string/join "\n")]
    (as-> (map vals records) it
          (map join-comma it)
          (join-new-line it))))