(ns boost-scraper.reports
  (:require [boost-scraper.utils :as utils]
            [datalevin.core :as d]
            [clojure.instant]
            [clojure.string :as str]
            [clojure.pprint :as pprint]))

(defn get-boost-summary-for-report [conn show-regex last-seen-timestamp]
  (d/q '[:find [?ballers ?boosts ?thanks ?summary ?stream_summary ?total_summary]
         :in $ ?regex ?last-seen-timestamp
         :where
         ;; find all invoices since last-seen for show-regex
         [(d/q [:find ?e
                :in $ ?regex' ?last-seen-timestamp'
                :where
                ;; find invoices after our last seen id
                ;; [?last :invoice/identifier ?last-seen']
                ;; [?last :invoice/creation_date ?last_creation_date]
                [?e :invoice/creation_date ?creation_date]
                [(< ?last-seen-timestamp' ?creation_date)]
                ;; filter out those troublemakers
                (not [?e :boostagram/sender_name_normalized "chrislas"])
                (not [?e :boostagram/sender_name_normalized "noblepayne"])
                ;; match our particular show
                [?e :boostagram/podcast ?podcast]
                [(get-else $ ?e :boostagram/episode "Unknown Episode") ?episode]
                (or [(re-matches ?regex' ?podcast) _]
                    [(re-matches ?regex' ?episode) _])]
               $ ?regex ?last-seen-timestamp)
          ?valid_eids]
         ;; aggregate boosts by sender_name_normalized
         [(d/q [:find ?sender_name_normalized (sum ?sats) (count ?e) (min ?d) (distinct ?e)
                :in $ [[?e] ...]
                :where
                [?e :boostagram/action "boost"]
                [?e :boostagram/sender_name_normalized ?sender_name_normalized]
                [?e :boostagram/value_sat_total ?sats]
                [?e :invoice/creation_date ?d]]
               $ ?valid_eids)
          ?sats_by_eid]
         ;; pull individual boost data for each sender
         [(d/q [:find ?sender_name_normalized' ?sat_total' ?boost_count' ?first_boost' ?boosts
                :in $ [[?sender_name_normalized' ?sat_total' ?boost_count' ?first_boost' ?boost_ids] ...]
                :where
                [(d/q [:find [(d/pull ?e' [:boostagram/sender_name_normalized
                                           :boostagram/value_sat_total
                                           :boostagram/podcast
                                           :boostagram/episode
                                           :boostagram/app_name
                                           :invoice/created_at
                                           :invoice/creation_date
                                           :invoice/identifier
                                           :boostagram/message]) ...]
                       :in $ [?e' ...]]
                      $ ?boost_ids)
                 ?boosts]]
               $ ?sats_by_eid)
          ?sats_by_eid_with_deets]
         ;;;; filter by report section
         ;; ballers
         [(d/q [:find ?sender_name_normalized' ?sat_total' ?boost_count' ?first_boost' ?boosts'
                :in $ [[?sender_name_normalized' ?sat_total' ?boost_count' ?first_boost' ?boosts'] ...]
                :where [(<= 20000 ?sat_total')]]
               $ ?sats_by_eid_with_deets)
          ?ballers]
         ;; boosts
         [(d/q [:find ?sender_name_normalized' ?sat_total' ?boost_count' ?first_boost' ?boosts'
                :in $ [[?sender_name_normalized' ?sat_total' ?boost_count' ?first_boost' ?boosts'] ...]
                :where
                [(<= 2000 ?sat_total')]
                [(< ?sat_total' 20000)]]
               $ ?sats_by_eid_with_deets)
          ?boosts]
         ;; thanks
         [(d/q [:find ?sender_name_normalized' ?sat_total' ?boost_count' ?first_boost' ?boosts'
                :in $ [[?sender_name_normalized' ?sat_total' ?boost_count' ?first_boost' ?boosts'] ...]
                :where
                [(< ?sat_total' 2000)]]
               $ ?sats_by_eid_with_deets)
          ?thanks]
         ;; boost summary
         [(d/q [:find [(sum ?sats) (count ?e) (count-distinct ?sender) (max ?creation_date)]
                :in $ [[?e] ...]
                :where
                ;; boost only
                [?e :boostagram/action "boost"]
                ;; bind our vars to aggregate
                [?e :invoice/creation_date ?creation_date]
                [?e :boostagram/value_sat_total ?sats]
                [?e :boostagram/sender_name_normalized ?sender]]
               $ ?valid_eids)
          ?summary]
         ;; stream summary
         [(d/q [:find [(sum ?sats) (count ?e) (count-distinct ?sender)]
                :in $ [[?e] ...]
                :where
                ;; streams only
                [?e :boostagram/action "stream"]
                ;; bind our vars to aggregate
                [?e :boostagram/sender_name_normalized ?sender]
                [?e :boostagram/value_sat_total ?sats]]
               $ ?valid_eids)
          ?stream_summary]
         ;; total summary
         [(d/q [:find [(sum ?sats) ?maxcd (count-distinct ?sender)]
                :in $ [[?e] ...] [_ _ _ ?maxcd]
                :where
                ;; bind our vars to aggregate
                [?e :invoice/creation_date ?creation_date]
                [?e :boostagram/sender_name_normalized ?sender]
                [?e :boostagram/value_sat_total ?sats]]
               $ ?valid_eids ?summary)
          ?total_summary]
          ;; TODO needed? find ID corresponding to max timestamp
         #_[(d/q [:find [?total_sat_sum ?last_seen_id ?distinct_senders]
                  :in $ [?total_sat_sum ?last_cd ?distinct_senders]
                  :where
                  [?e :invoice/creation_date ?last_cd]
                  [?e :invoice/identifier ?last_seen_id]]
                 $ ?total_summary_p1)
            ?total_summary]]
       (d/db conn) show-regex last-seen-timestamp))

(defn sort-report
  [[ballers
    boosts
    thanks
    [boost_total_sats boost_total_boosts boost_total_boosters]
    [stream_total_sats stream_total_streams stream_total_streamers]
    [total_sats last_seen_id total_unique_boosters]]]
  (letfn [(sort-boosts [[sender total count mindate boosts]]
            {:sender sender
             :total total
             :count count
             :mindate mindate
             :boosts (sort-by :invoice/created_at boosts)})]
    {:ballers (sort-by :total #(compare %2 %1) (map sort-boosts ballers))
     :boosts (sort-by :mindate (map sort-boosts boosts))
     :thanks (sort-by :mindate (map sort-boosts thanks))
     :boost-summary {:boost_total_sats boost_total_sats
                     :boost_total_boosts boost_total_boosts
                     :boost_total_boosters boost_total_boosters}
     :stream-summary {:stream_total_sats stream_total_sats
                      :stream_total_streams stream_total_streams
                      :stream_total_streamers stream_total_streamers}
     :summary {:total_sats total_sats
               :last_seen_id last_seen_id
               :total_unique_boosters total_unique_boosters}}))

(defn int-comma [n] (clojure.pprint/cl-format nil "~:d"  (float (or n 0))))

(defn format-boost-batch-details [[boost & batch]]
  (str/join
   "\n"
   (concat
    (let [{:keys [boostagram/message
                  boostagram/value_sat_total
                  boostagram/podcast
                  boostagram/episode
                  boostagram/app_name
                  #_invoice/identifier
                  invoice/creation_date]} boost]
      [(str "+ " podcast "\n"
            "+ " episode "\n"
            "+ " app_name "\n"
            #_("+ " identifier "\n")
            "\n"
            "+ " (utils/format-date creation_date) " (" creation_date ")" "\n"
            "+ " (int-comma value_sat_total) " sats\n"
            (str/join "\n" (map #(str "> " %) (str/split-lines (or message "No Message Found :(")))))])
    (for [{:keys [boostagram/message boostagram/value_sat_total
                  invoice/creation_date
                  #_invoice/identifier]} batch]
      (str "\n"
           #_("+ " identifier "\n")
           "+ " (utils/format-date creation_date) " (" creation_date ")" "\n"
           "+ " (int-comma value_sat_total) " sats\n"
           (str/join "\n" (map #(str "> " %) (str/split-lines (or message "No Message Found :(")))))))))

(defn format-boost-batch [{:keys [sender total count boosts]}]
  (str "### From: " sender "\n"
       "+ " (int-comma total) " sats\n"
       "+ " (int-comma count) " boosts\n"
       (format-boost-batch-details boosts)
       "\n"))

(defn format-boost-section [boosts]
  (str/join "\n" (map format-boost-batch boosts)))

(defn format-sorted-report
  [{:keys [ballers boosts thanks boost-summary stream-summary summary]}]
  (str "## Baller Boosts\n"
       (format-boost-section ballers) "\n"
       "## Boosts\n"
       (format-boost-section boosts) "\n"
       "## Thanks\n"
       (format-boost-section thanks)
       "\n## Boost Summary"
       "\n+ Total Sats: " (int-comma (:boost_total_sats boost-summary))
       "\n+ Total Boosts: " (int-comma (:boost_total_boosts boost-summary))
       "\n+ Total Boosters: " (int-comma (:boost_total_boosters boost-summary))
       "\n"
       "\n## Stream Summary"
       "\n+ Total Sats: " (int-comma (:stream_total_sats stream-summary))
       "\n+ Total Streams: " (int-comma (:stream_total_streams stream-summary))
       "\n+ Total Streamers: " (int-comma (:stream_total_streamers stream-summary))
       "\n"
       "\n## Summary"
       "\n+ Total Sats: " (int-comma (:total_sats summary))
       "\n+ Total Boosters: " (int-comma (:total_unique_boosters summary))
       "\n"
       "\n## Last Seen"
       "\n+ Last seen ID: " (:last_seen_id summary)
       "\n"))

(defn boost-report [conn show-regex last-seen-id]
  (->> (get-boost-summary-for-report conn show-regex last-seen-id)
       sort-report
       format-sorted-report))

