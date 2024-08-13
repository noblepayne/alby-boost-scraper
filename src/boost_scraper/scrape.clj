(ns boost-scraper.scrape
  (:require [cheshire.core :as json]
            [datalevin.core :as d]
            [clojure.instant]
            [clojure.string :as str]))

#_(def dbi "/media/wes/a8dc84bb-2377-490e-a4b3-027425850e03/workdir/boosts/boostdb")
(def alby-dbi "/home/wes/Downloads/boostdb/alby_boostdb")
(def lnd-dbi "/home/wes/Downloads/boostdb/lnd_boostdb")

(def schema
  {:invoice/identifier {:db/valueType :db.type/string
                        :db/unique :db.unique/identity}
   :invoice/source {:db/valueType :db.type/string}
   :invoice/creation_date {:db/valueType :db.type/long}
   :invoice/creation_date_per_30 {:db/valueType :db.type/long}
   :invoice/created_at {:db/valueType :db.type/instant}
   :invoice/add_index {:db/valueType :db.type/long}
   :boostagram/sender_name {:db/valueType :db.type/string}
   :boostagram/sender_name_normalized {:db/valueType :db.type/string}
   :boostagram/episode {:db/valueType :db.type/string}
   :boostagram/podcast {:db/valueType :db.type/string}
   :boostagram/app_name {:db/valueType :db.type/string}
   :boostagram/action {:db/valueType :db.type/string}
   :boostagram/message {:db/valueType :db.type/string}
   :boostagram/value_msat_total {:db/valueType :db.type/long}
   :boostagram/value_sat_total {:db/valueType :db.type/long}}
    ;;
    ;; :table/column {:db/valueType :db.type/...}
  )

(defn remove-nil-vals [m]
  (->> m
       (remove
        (fn [[_ v]]
          (or (nil? v)
              (and (string? v)
                   (empty? v)))))
       (into {})))

(defn namespace-invoice-keys [toplvl-key m]
  (reduce
   (fn [xs [k v]]
     (if (map? v)
       (assoc xs k v)
       (let [toplvl (get xs toplvl-key {})
             toplvl (assoc toplvl k v)]
         (assoc xs toplvl-key toplvl))))
   {}
   m))

(defn flatten-paths
  "https://andersmurphy.com/2019/11/30/clojure-flattening-key-paths.html"
  [separator m]
  (letfn [(flatten-paths [m separator path]
            (lazy-seq
             (when-let [[[k v] & xs] (seq m)]
               (cond (and (map? v) (not-empty v))
                     (into (flatten-paths v separator (conj path k))
                           (flatten-paths xs separator path))
                     :else
                     (cons [(->> (conj path k)
                                 (map name)
                                 (clojure.string/join separator)
                                 keyword) v]
                           (flatten-paths xs separator path))))))]
    (into {} (flatten-paths m separator []))))

(defn normalize-name [name_]
  (-> name_
      str/trim
      str/lower-case
      ((fn [s] (if (str/starts-with? s "@")
                 (.substring s 1)
                 s)))))

(defn decode-boost [rawboost]
  (try
    (let [decoder (java.util.Base64/getDecoder)]
      (-> decoder
          (.decode rawboost)
          (#(java.lang.String. %))
          json/parse-string
          remove-nil-vals
          (#(namespace-invoice-keys :boostagram %))
          (#(flatten-paths "/" %))))
    (catch Exception e (println "EXCEPTION DECODING BOOST: " rawboost) {})))

(defn coerce-invoice-vals [invoice]
  (let [;; From LND; if present use as string identifier.
        invoice (if-let [add_index (get invoice :invoice/add_index)]
                  (assoc
                   invoice
                   :invoice/identifier
                   add_index)
                  invoice)
        ;; From LND; keep a copy as an int for queries.
        invoice (if-let [add_index (get invoice :invoice/add_index)]
                  (assoc
                   invoice
                   :invoice/add_index
                   (Integer/parseInt add_index))
                  invoice)
        ;; Convert string creation_date (unix epoch) into int for indexes/queries.
        invoice (if-let [creation_date (get invoice :invoice/creation_date)]
                  (assoc
                   invoice
                   :invoice/creation_date
                   (if (string? creation_date) (Integer/parseInt creation_date) creation_date))
                  invoice)
        #_invoice #_(if-let [creation_date (get invoice :invoice/creation_date)]
                      (assoc
                       invoice
                       :invoice/creation_date_per_30
                       (if (= "boost" (get invoice :boostagram/action))
                         (quot creation_date 30)
                         (rand-int 1000000000)))
                      invoice)
        ;; From Alby; if present parse from string into Date.
        invoice (if-let [created_at (get invoice :invoice/created_at)]
                  (assoc
                   invoice
                   :invoice/created_at
                   (clojure.instant/read-instant-date created_at))
                  invoice)
        ;; If created_at is not present, create it form creation_date.
        invoice (if-not (contains? invoice :invoice/created_at)
                  (if-let [creation_date (get invoice :invoice/creation_date)]
                    (assoc
                     invoice
                     :invoice/created_at
                     (java.util.Date/from (java.time.Instant/ofEpochSecond creation_date)))
                    invoice)
                  invoice)
        ;; From LND; if present parse into boostagram data.
        invoice (if-let [[{{rawboost :7629169} :custom_records} :as debug] (get invoice :invoice/htlcs)]
                  (do #_(println debug "\n")
                   (into
                    (dissoc invoice :invoice/htlcs)
                    (decode-boost rawboost)))
                  invoice)
        ;; Noramlize sender name.
        invoice (if-let [sender_name (get invoice :boostagram/sender_name)]
                  (assoc
                   invoice
                   :boostagram/sender_name_normalized
                   (normalize-name sender_name))
                  invoice)
        ;; Convert millisats to sats.
        invoice (if-let [msats (get invoice :boostagram/value_msat_total)]
                  (assoc
                   invoice
                   :boostagram/value_sat_total
                   (/ msats 1000))
                  invoice)]
    invoice))

(defn process-batch [batch]
  (into []
        (comp (map #(dissoc % :features))
              (map #(dissoc % :amp_invoice_state))
              (map #(dissoc % :metadata))
              (map #(dissoc % :custom_records))
              #_(map #(println %))
              (map #(namespace-invoice-keys :invoice %1))
              (map #(flatten-paths "/" %1))
              (map remove-nil-vals)
              (map coerce-invoice-vals))
        #_(map #(into (sorted-map) %))
        batch))

(defn add-boosts [conn boosts]
  (->> boosts
       (map process-batch)
       (run! (fn [boost-batch]
               (d/transact! conn boost-batch)))))