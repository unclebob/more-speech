(ns more-speech.db.in-memory
  (:require [more-speech.db.gateway :as gateway]))

(defmethod gateway/add-profile ::type [db id profile]
  (swap! (:data db) update-in [:profiles] assoc id profile))

(defmethod gateway/get-profile ::type [db id]
  (get-in @(:data db) [:profiles id]))

(defmethod gateway/event-exists? ::type [db id]
  (contains? (:text-event-map @(:data db)) id))

(defmethod gateway/add-event ::type [db id event]
  (swap! (:data db) assoc-in [:text-event-map id] event))

(defmethod gateway/get-event ::type [db id]
  (get-in @(:data db) [:text-event-map id]))

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

(defmethod gateway/get-contacts ::type [db user-id]
  (get-in @(:data db) [:contact-lists user-id]))

(defmethod gateway/get-id-from-petname ::type [db user-id petname]
  (loop [contacts (gateway/get-contacts db user-id)]
    (if (seq contacts)
      (if (= petname (:petname (first contacts)))
        (:pubkey (first contacts))
        (recur (rest contacts)))
      nil)))

(defmethod gateway/get-id-from-username ::type [db user-name]
  (let [profiles (:profiles @(:data db))]
    (loop [pairs (vec profiles)]
      (if (empty? pairs)
        nil
        (let [pair (first pairs)]
          (if (= user-name (:name (second pair)))
            (first pair)
            (recur (rest pairs))))))))

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
