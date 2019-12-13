(ns exercises.adventofcode.intcode-computer)


; Helper
(defn get-empty-memory-map []
  {:input 0 :outputs [] :memory [] :instruction-pointer 0 :op-code-size 5})


; Operations on memory-map
(defn immediate-mode [{memory :memory, instruction-pointer :instruction-pointer} arg-index]
  (nth memory (+ instruction-pointer arg-index)))

(defn position-mode->address [memory-map arg-index]
  (immediate-mode memory-map arg-index))

(defn position-mode->value [{memory :memory, :as memory-map} arg-index]
  (nth memory (position-mode->address memory-map arg-index)))

(defn assoc-in-memory [memory-map position value]
  (assoc-in memory-map [:memory position] value))

(defn jump [amount {instruction-pointer :instruction-pointer, :as memory-map}]
  (assoc-in memory-map [:instruction-pointer] (+ instruction-pointer amount)))

(defn- operation-mode->fn [mode key]
  (get-in
    {\0 {:address position-mode->address :value position-mode->value}
     \1 {:address position-mode->address :value immediate-mode}}
    [mode key]))


; Op Codes Operations
(defn standard [fun memory-map get-param]
  (->> (assoc-in-memory memory-map (get-param :address 2) (fun (get-param :value 0) (get-param :value 1)))
       (jump 4)))

(defn input [{input :input, :as memory-map} get-param]
  (->> (assoc-in-memory memory-map (get-param :address 0) input)
       (jump 2)))

(defn output [memory-map get-param]
  (->> (update-in memory-map [:outputs] #(conj % (get-param :value 0)))
       (jump 2)))

(defn jump-if-true [memory-map get-param]
  (if (not= (get-param :value 0) 0)
    (assoc-in memory-map [:instruction-pointer] (get-param :value 1))
    (jump 3 memory-map)))

(defn jump-if-false [memory-map get-param]
  (if (= (get-param :value 0) 0)
    (assoc-in memory-map [:instruction-pointer] (get-param :value 1))
    (jump 3 memory-map)))

(defn less-than [memory-map get-param]
  (->> (if (< (get-param :value 0) (get-param :value 1))
         (assoc-in-memory memory-map (get-param :address 2) 1)
         (assoc-in-memory memory-map (get-param :address 2) 0))
       (jump 4)))

(defn equals [memory-map get-param]
  (->> (if (= (get-param :value 0) (get-param :value 1))
         (assoc-in-memory memory-map (get-param :address 2) 1)
         (assoc-in-memory memory-map (get-param :address 2) 0))
       (jump 4)))


; Get op code and modes
(defn- get-to-expected-size [op-code expected-size]
  (let [remaining (- expected-size (count (str op-code)))]
    (str
      (apply str (take remaining (repeat "0")))
      op-code)))

(defn- get-code [op-code-mode expected-size]
  (str (nth op-code-mode (- expected-size 2)) (nth op-code-mode (- expected-size 1))))

(defn- get-mode [op-code-mode expected-size]
  (loop [result [], remaining (- expected-size 2)]
    (if (= 0 remaining)
      result
      (let [index (dec remaining)
            mode (nth op-code-mode index)]
        (recur (conj result (partial operation-mode->fn mode)) index)))))

(defn get-op-code [{memory :memory, instruction-pointer :instruction-pointer op-code-size :op-code-size}]
  (let [op-code-mode (get-to-expected-size (nth memory instruction-pointer) op-code-size)]
    {:operation-code (get-code op-code-mode op-code-size),
     :operation-mode (get-mode op-code-mode op-code-size)}))


(defn operation-modes->value [{operation-modes :operation-mode} memory-map key arg-index]
  (let [operation-mode ((nth operation-modes arg-index) key)]
    (operation-mode memory-map (inc arg-index))))


(defn evaluate
  ([memory-map] "Finds op-code and iterates or stops"
   (let [op-code (get-op-code memory-map)
         is-operation-code? (partial = (:operation-code op-code))
         get-param (partial operation-modes->value op-code memory-map)]
     (cond
       (is-operation-code? "99") memory-map
       (is-operation-code? "01") (recur (standard + memory-map get-param))
       (is-operation-code? "02") (recur (standard * memory-map get-param))
       (is-operation-code? "03") (recur (input memory-map get-param))
       (is-operation-code? "04") (recur (output memory-map get-param))
       (is-operation-code? "05") (recur (jump-if-true memory-map get-param))
       (is-operation-code? "06") (recur (jump-if-false memory-map get-param))
       (is-operation-code? "07") (recur (less-than memory-map get-param))
       (is-operation-code? "08") (recur (equals memory-map get-param))
       true (println (str "wrong op-code " op-code))
       ))))