(ns exercises.core
  (:import java.net.URI)
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "I'm a little teapot!"))

(defn- extract-endpoint
  [uri]
  (str (.getScheme uri)
       "://"
       (.getAuthority uri)))

(defn- extract-signing-region
  [uri]
  (let [authority (.getAuthority uri)]
    (if (and (.startsWith authority "sqs.")
             (.endsWith authority ".amazonaws.com"))
      (-> authority
          (.replace "sqs." "")
          (.replace ".amazonaws.com" ""))
      "us-east-1")))

(defn- extract-queue-name
  [uri]
  (let [path (.getPath uri)
        last-slash-idx (.lastIndexOf path "/")]
    (.substring path (inc last-slash-idx))))

(defn queue-url-to-map
  "Extract endpoint, signing-region and queue-name from an SQS queue URL.
  If the URL isn't from AWS (.ie localstack) then the signing-region
  default to us-east-1."
  [queue-url]
  (->> (URI. queue-url)
       ((juxt extract-endpoint
              extract-signing-region
              extract-queue-name))
       (zipmap [:endpoint :signing-region :queue-name])))

(defn queue-url-to-map-2
  "Extract endpoint, signing-region and queue-name from an SQS queue URL.
  If the URL isn't from AWS (.ie localstack) then the signing-region
  default to us-east-1."
  [queue-url]
  (let [uri (URI. queue-url)]
    (let [extract-all (juxt extract-endpoint extract-signing-region extract-queue-name)]
      (-> (zipmap [:endpoint :signing-region :queue-name] (extract-all uri))))))
