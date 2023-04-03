(ns more-speech.db.in-memory
  (:require [more-speech.db.gateway :as gateway]))

(defmethod gateway/add-profile ::type [db id profile]
  (swap! (:data db) update-in [:profiles] assoc id profile))

(defmethod gateway/add-profiles-map ::type [db profiles-map]
  (loop [ids (keys profiles-map)]
    (if (empty? ids)
      nil
      (let [id (first ids)]
        (gateway/add-profile db id (get profiles-map id))
        (recur (rest ids))))))

(defmethod gateway/get-profile ::type [db id]
  (get-in @(:data db) [:profiles id]))

(defmethod gateway/event-exists? ::type [db id]
  (contains? (:text-event-map @(:data db)) id))

(defmethod gateway/add-event ::type [db event]
  (let [id (:id event)]
    (swap! (:data db) assoc-in [:text-event-map id] event)))

(defmethod gateway/add-events ::type [db events]
  (when-let [event (first events)]
    (gateway/add-event db event)
    (recur db (rest events))))

(defmethod gateway/get-event ::type [db id]
  (get-in @(:data db) [:text-event-map id]))

(defmethod gateway/get-some-recent-event-authors ::type [db after]
  (loop [ids (keys (:text-event-map @(:data db)))
         result #{}]
    (if (empty? ids)
      result
      (let [id (first ids)
            event (gateway/get-event db id)
            author (:pubkey event)
            time (:created-at event)]
        (if (and (some? time) (> time after))
          (recur (rest ids) (conj result author))
          (recur (rest ids) result))))))

(defmethod gateway/get-ids-by-author-since ::type [db author start-time]
  (let [event-map (:text-event-map @(:data db))
        ids (keys event-map)
        events (map #(gateway/get-event db %) ids)
        authors-events (filter #(= author (:pubkey %)) events)
        authors-since-events (filter #(> (:created-at %) start-time) authors-events)]
    (map :id authors-since-events))
  )
(defn cited? [id tags]
  (some #(= id (second %)) tags))

(defmethod gateway/get-ids-that-cite-since ::type [db id start-time]
  (let [event-map (:text-event-map @(:data db))
        ids (keys event-map)
        events (map #(gateway/get-event db %) ids)
        cited-events (filter #(cited? id (:tags %)) events)
        authors-since-events (filter #(> (:created-at %) start-time) cited-events)]
    (map :id authors-since-events)))

(defmethod gateway/update-event-as-read ::type [db id]
  (swap! (:data db) assoc-in [:text-event-map id :read] true))

(defmethod gateway/add-relays-to-event ::type [db id relays]
  (let [event (gateway/get-event db id)
        updated-relays (set (concat (:relays event) relays))]
    (swap! (:data db) assoc-in [:text-event-map id :relays] updated-relays)))

(defmethod gateway/add-reference-to-event ::type [db id reference]
  (swap! (:data db)
         update-in [:text-event-map id :references] conj reference))

(defmethod gateway/add-contacts ::type [db user-id contacts]
  (swap! (:data db) assoc-in [:contact-lists user-id] contacts))

(defmethod gateway/add-contacts-map ::type [db contacts-map]
  (loop [ids (keys contacts-map)]
    (if (empty? ids)
      nil
      (let [id (first ids)]
        (gateway/add-contacts db id (get contacts-map id))
        (recur (rest ids))))))

(defmethod gateway/get-contacts ::type [db user-id]
  (get-in @(:data db) [:contact-lists user-id]))

(defmethod gateway/get-id-from-username ::type [db user-name]
  (let [profiles (:profiles @(:data db))]
    (loop [pairs (vec profiles)]
      (if (empty? pairs)
        nil
        (let [pair (first pairs)]
          (if (= user-name (:name (second pair)))
            (first pair)
            (recur (rest pairs))))))))

(defn conj-set [s1 val]
  (conj (set s1) val))

(defmethod gateway/add-reaction ::type [db id pubkey content]
  (swap! (:data db)
         update-in [:text-event-map id :reactions] conj-set [pubkey content]))

(defmethod gateway/get-profiles-after ::type [db after]
  (loop [ids (keys (:profiles @(:data db)))
         result []]
    (if (empty? ids)
      result
      (let [id (first ids)
            profile (gateway/get-profile db id)
            time (:created-at profile)]
        (if (> time after)
          (recur (rest ids) (conj result [id (:name profile)]))
          (recur (rest ids) result))))))

;----------methods for tests
(def db (atom nil))

(defn get-db [] {:data db ::gateway/type ::type})

(defn clear-events [db]
  (swap! (:data db) assoc :text-event-map {}))

(defn clear-profiles [db]
  (swap! (:data db) assoc :profiles {}))

(defn clear-contacts [db]
  (swap! (:data db) assoc :contact-lists {}))

(defn clear-db [db]
  (clear-events db)
  (clear-profiles db)
  (clear-contacts db))
