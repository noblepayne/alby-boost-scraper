(ns boost-scraper.scrape
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [babashka.cli :as cli]
            [clojure.string :as str]
            [clojure.pprint]
            [clojure.edn]))

(def alby-incoming-invoices-url "https://api.getalby.com/invoices/incoming")
(def alby-token-refresh-url "https://api.getalby.com/oauth/token")

(def show-by-id {:lup "LINUX Unplugged"
                 :lan "Linux Action News"
                 :ssh "Self-Hosted"
                 :coder "Coder Radio"
                 :office "Office Hours 2.0"
                 :bdadpod "Bitcoin Dad Pod"
                 :unfilter "Unfilter"
                 :twib "This Week in Bitcoin"})

(def id-by-show (into {} (map (comp vec reverse) show-by-id)))

(defn get-new-auth-token [basic-auth-secret refresh-token]
  (-> alby-token-refresh-url
      (http/post
       {:headers {:authorization (str "Basic " basic-auth-secret)}
        :form-params {"refresh_token" refresh-token
                      "grant_type" "refresh_token"}})
      :body
      (json/parse-string true)))

(defn get-boosts [{:keys [:token :items :page :after]}]
  (let [query-params {:items items :page page}
        query-params (if after
                       (assoc query-params
                              "q[created_at_gt]"
                              (/ (.getTime after) 1000))
                       query-params)
        data (-> alby-incoming-invoices-url
                 (http/get {:headers {:authorization (str "Bearer " token)}
                            :query-params query-params})
                 :body
                 (json/parse-string true))]
    {:next {:token token
            :items items
            :page (inc page)
            :after after}
     :data data}))


(defn get-all-boosts [token items-per-page]
  (iteration get-boosts
             {:somef (comp seq :data)
              :vf :data
              :kf :next
              :initk {:token token
                      :items items-per-page
                      :page 1
                      #_(comment :after last-lup)}}))

(def filter-boosts
  (comp (filter #(= "boost" (-> % :boostagram :action)))
        (map #(assoc (:boostagram %)
                     :creation_date (:creation_date %)
                     :identifier (:identifier %)))))

(defn get-new-boosts
  ([token last-boost-id] (get-new-boosts token last-boost-id (map identity)))
  ([token last-boost-id boost-filter]
   (sort-by :creation_date
            (into []
                  (comp cat
                        filter-boosts
                        boost-filter
                        (take-while #(not= last-boost-id (:identifier %))))
                  (get-all-boosts token 100)))))

(defn get-n-boosts
  ([token n] (get-n-boosts token n (map identity)))
  ([token n boost-filter]
   (sort-by :creation_date
            (into []
                  (comp cat
                        #_(remove (comp empty? :memo))
                        filter-boosts
                        boost-filter
                        (take n))
                  (get-all-boosts token 100)))))

(defn format-date [unix-time]
  (-> (java.time.Instant/ofEpochSecond unix-time)
      (.atZone (java.time.ZoneId/of "America/Los_Angeles"))
      (.format (java.time.format.DateTimeFormatter/ofPattern "yyyy/MM/dd h:mm:ss a zzz"))))

(defn int-comma [n] (clojure.pprint/cl-format nil "~:d"  (float n)))

(defn format-boost [{:keys [podcast
                            sender_name
                            value_msat_total
                            ;; :ts
                            message
                            episode
                            app_name
                            creation_date
                            identifier]}]
  (let [sats (int-comma (/ value_msat_total 1000))
        date (format-date creation_date)]
    (str
     "### From: " (or sender_name "no name provided :(")
     "\n" "+ " sats " sats"
     "\n" "+ " podcast
     "\n" "+ " episode
     "\n" "+ " app_name
     "\n" "+ " date
     "\n" "+ " identifier
     "\n\n" (str/join
             "\n"
             (map
              (fn [x]
                (let [x (str/trim x)]
                  (if (seq x)
                    (str "> " x) x)))
              (str/split-lines (or message "no message provided :("))))
     "\n\n")))

(defn filter-by-show [show]
  (let [show-name (get show-by-id show)]
    (filter #(or (= show-name  (:podcast %))
                 (str/includes? (get % :episode "") show-name)))))

(defn format-summarized-boost-by-sender [[sender_name {:keys [sats creation_date boosts count]}]]
  (let [total-sats (int-comma sats)
        date (format-date creation_date)
        {:keys [podcast episode app_name identifier]} (first boosts)
        sats-n-messages (map (juxt #(int-comma (/ (:value_msat_total %) 1000)) :message) boosts)]
    (str
     "### From: " sender_name
     "\n" "+ " total-sats " sats"
     (when (< 1 count) (str "\n" "+ " count " boosts"))
     "\n" "+ " podcast
     "\n" "+ " episode
     "\n" "+ " app_name
     "\n" "+ " date
     "\n" "+ " identifier
     "\n\n" (str/join "\n\n"
                      ;; TODO: clean up
                      (map (fn [[sats message]]
                             (str
                              (when (< 1 count)
                                (str "+ " sats " sats"
                                     "\n"))
                              (str/join
                               "\n"
                               (map
                                (fn [x]
                                  (let [x (str/trim x)]
                                    (if (seq x)
                                      (str "> " x) x)))
                                (str/split-lines (or message "no message provided :("))))))
                           sats-n-messages))
     "\n\n")))

(defn format-boosts
  [boosts]
  (->> boosts
       (map format-boost)
       str/join
       print))

(defn boosts-by-sender [boosts]
  (reduce
   (fn [state new]
     (let [sender_name (get new :sender_name "unknown_sender")
           current-value (get state sender_name [])
           new-value (conj current-value new)]
       (assoc state sender_name new-value)))
   {}
   boosts))

(defn summarize-boosts-by-sender [boosts-by-sender]
  (into {}
        (map (fn [[sender boosts]]
               [sender {:count (count boosts)
                        :sats (/ (reduce
                                  #(+ %1 (get %2 :value_msat_total 0))
                                  0
                                  boosts)
                                 1000)
                        :boosts (sort-by :creation_date boosts)
                        :creation_date (apply min (map :creation_date boosts))}]))

        boosts-by-sender))

(defn boost-report
  [boosts]
  (let [; new-boosts (->> last-seen-boost-id
        ;                 (get-new-boosts token)
        ;                 (sort-by :creation_date))
        new-boosts boosts
        new-last-id (->> new-boosts last :identifier)
        boosts-by-sender (summarize-boosts-by-sender
                          (boosts-by-sender new-boosts))
        total-sats (/ (reduce + (map :value_msat_total new-boosts)) 1000)
        ballers (->> boosts-by-sender
                     (filter #(<= 20000 (:sats (second %))))
                     (sort-by (comp (juxt (comp - :sats) :creation_date) second)))
        boosters (->> boosts-by-sender
                      (filter #(<= 2000 (:sats (second %)) 19999))
                      (sort-by (comp (juxt :creation_date (comp - :sats)) second)))
        thanks (->> boosts-by-sender
                    (filter #(> 2000 (:sats (second %))))
                    (sort-by (comp (juxt :creation_date (comp - :sats)) second)))
        ; formatted-ballers (->> ballers (mapcat #(-> % second :boosts)) (map format-boost) str/join)
        ; formatted-boosters (->> boosters (mapcat #(-> % second :boosts)) (map format-boost) str/join)
        ; formatted-thanks (->> thanks (mapcat #(-> % second :boosts)) (map format-boost) str/join)]

        formatted-ballers (->> ballers (map format-summarized-boost-by-sender) str/join)
        formatted-boosters (->> boosters (map format-summarized-boost-by-sender) str/join)
        formatted-thanks (->> thanks (map format-summarized-boost-by-sender) str/join)]
    (str
     "## Baller Boosts\n"
     formatted-ballers
     "## Boosts\n"
     formatted-boosters
     "## Thanks\n"
     formatted-thanks
     "### Totals\n"
     "+ Total Sats: " (int-comma total-sats) "\n"
     "+ Total Boosts: " (int-comma (count new-boosts)) "\n"
     "+ Total Boosters: " (int-comma (count boosts-by-sender)) "\n"
     "\n"
     "### Last Seen Boost\n"
     "Last seen boost id: " new-last-id
     "\n\n")))

(defn read-show-key [raw-show]
  (let [parsed-show (clojure.edn/read-string (str raw-show))
        parsed-show (if (keyword? parsed-show) parsed-show raw-show)
        show (cond
               (contains? show-by-id parsed-show) parsed-show
               (contains? id-by-show parsed-show) (get id-by-show parsed-show))]
    (if (nil? show)
      (throw (ex-info "Unknown show!" {:raw-show raw-show :parsed-show parsed-show}))
      show)))

(defn autoscrape [& args]
  (let [raw-show (str/join " " args)
        show (read-show-key raw-show)
        {:keys [basic-auth-secret refresh-token last-ids] :as secrets}
        (clojure.edn/read-string (slurp "decrypted_secrets"))
        ;; _ (println basic-auth-secret "" refresh-token)
        {:keys [refresh_token access_token]} (get-new-auth-token basic-auth-secret refresh-token)]
    ;; (println refresh_token access_token)
    (spit "decrypted_secrets" (merge secrets
                                     {:refresh-token refresh_token}))
    (println (->> (get-new-boosts access_token
                                  (get last-ids show)
                                  (filter-by-show show))
                  (boost-report)))))


(defn update-last-id [& args]
  (let [[last-id & show-args] (reverse args)
        show (str/join " " (reverse show-args))
        show (read-show-key show)
        {:keys [last-ids] :as secrets}
        (clojure.edn/read-string (slurp "decrypted_secrets"))
        last-ids (merge last-ids {show last-id})]
    (spit "decrypted_secrets" (merge secrets {:last-ids last-ids}))))



(def cli-opts
  {:boosts {:alias :n
            :desc "Number of boosts to fetch."
            :default 10
            :coerce :int}
   :token {:alias :t
           :desc "Alby oauth token."
           :require true
           :coerce :string}})

(defn -main [& _]
  (try
    (let [opts (cli/parse-opts *command-line-args* {:spec cli-opts :args->opts [:token :boosts]})]
      (->> (get-n-boosts (:token opts) (:boosts opts)) format-boosts))
    (catch Exception e
      (if (= :org.babashka/cli (-> e bean :data :type))
        (do
          (println
           (first (str/split (str e) #"\{")) "\n")

          (println "Help:")

          (println
           (cli/format-opts
            {:spec cli-opts :order [:token :boosts]})))
        (println (.getMessage e)))
      (System/exit 1))))

(comment

  (require '[portal.api :as p])
  (p/open)
  (add-tap #'p/submit)


  (def test-token "NGY3ZGEWMJYTMTY0OS0ZNMI2LWFIYMITYMMWMDQXNZQ0NDDI")
  (def last-lup #inst "2023-03-27T12-07:00")

  (->> (get-boosts
        {:token test-token :items 50 :page 5 :after last-lup})
       :data
       (remove #(or (= "stream" (-> % :boostagram :action))
                    (= "streaming" (-> % :boostagram :action))))
       tap>)

  (->> 10 (get-n-boosts test-token) #_tap> clojure.pprint/pprint)
  (->> 100 (get-n-boosts test-token)
       ; (filter #(= (:podcast %) "All Jupiter Broadcasting Shows")) tap>)
       (filter #(= (:podcast %) "This Week in Bitcoin")) boost-report println)

  (->> (filter-by-show :lup) (get-n-boosts test-token 30) format-boosts)
  (->> (filter-by-show :coder) (get-n-boosts test-token 30) format-boosts)
  (->> (filter-by-show :bdadpod) (get-n-boosts test-token 10) format-boosts)

  (->> (get-new-boosts test-token "") tap>)

  (->> (filter-by-show :coder)
       (get-new-boosts test-token "xmf9GD1hDCtro3LctcTEdEE8")
       boost-report
       (spit "/tmp/new-boosts.md"))

  (def res (get-new-boosts test-token "Y5DPwG6PzuYniZUTe57ot4SU"))

  (-> res boosts-by-sender summarize-boosts-by-sender)


  (boost-report (get-new-boosts test-token ""))

  (autoscrape :lup))


;; old shit

;; (defn format-date-old-n-busted [unix_time]
;;   (let [date (java.util.Date. (* 1000 unix_time))
;;         formatter (java.text.SimpleDateFormat. "MM/dd/yyyy h:mm:ss aa zzz")
;;         timezone (.getTimeZone java.util.TimeZone "America/Los_Angeles")
;;         _ (.setTimeZone formatter timezone)]
;;     (.format formatter date)))
