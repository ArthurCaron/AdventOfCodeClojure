(ns exercises.interpolation
  (:require [clojure.string :as str]))

(def user-input "[[contains?]] {{doc.title}} contains?")

(def doc-context
  {:doc {
         :title  "Kinder surprise!"
         :fields [1 2 3 4 5]}})

(def funcs-context
  {
   :contains? str/includes?
   }
  )

(defn extract [context element-key]
  (let [splitted-elements (str/split element-key #"\.")
        splitted-keys (map keyword splitted-elements)]
    (get-in context splitted-keys)))

(defn replace-data [context element]
  (let [data-key (get (re-matches #"\{\{(.*)\}\}" element) 1)]
    (if (nil? data-key)
      element
      (extract context data-key)
      )))

(defn replace-funcs [available-funcs element]
  (let [func-key (get (re-matches #"\[\[(.*)\]\]" element) 1)]
    (if (nil? func-key)
      element
      (extract available-funcs func-key)
      )))

(defn evaluate [context input]
  (let [splitted-input (str/split input #" ")]
    (->> splitted-input
         (map (partial replace-data context))
         (map (partial replace-funcs funcs-context))
         (eval)
         )))


(def eval-test (partial evaluate doc-context))

(def test-eval (eval-test user-input))