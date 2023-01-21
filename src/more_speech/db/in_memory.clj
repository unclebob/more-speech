(ns more-speech.db.in-memory
  (:require [more-speech.db.gateway :as gateway]))

(defmethod gateway/add-profile ::type [db id profile]
  (swap! (:data db) update-in [:profiles] assoc id profile))

(defmethod gateway/add-text-event ::type [db id text-event]
  )
