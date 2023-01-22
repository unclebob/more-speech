(ns more-speech.db.in-memory
  (:require [more-speech.db.gateway :as gateway]))

(defmethod gateway/add-profile ::type [db id profile]
  (swap! (:data db) update-in [:profiles] assoc id profile))

(defmethod gateway/event-exists? ::type [db id]
  (contains? (:text-event-map @(:data db)) id))

(defmethod gateway/add-event ::type [db id event]
  (let [time (:created-at event)]
    (swap! (:data db) assoc-in [:text-event-map id] event)
    (swap! (:data db) update-in [:chronological-text-events] conj [id time])))

(defmethod gateway/add-relays-to-event ::type [db id relays]
  (swap! (:data db) update-in [:text-event-map id :relays] concat relays))

(defmethod gateway/add-reference-to-event ::type [db id reference]
  (swap! (:data db)
         update-in [:text-event-map id :references] conj reference))
