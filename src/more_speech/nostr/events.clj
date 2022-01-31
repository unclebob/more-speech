(ns more-speech.nostr.events
  (:require [more-speech.article :as article]
            [clojure.data.json :as json]
            [more-speech.nostr.util :refer [hex-string->num]]))

(declare process-text-event)

(defn process-event [{:keys [application] :as state} event]
  (let [{:keys [articles nicknames]} application
        name-of (fn [pubkey] (get nicknames pubkey pubkey))
        [name subscription-id inner-event :as decoded-msg] event
        {:strs [id pubkey created_at kind tags content sig]} inner-event]
    (condp = kind
      0 (update-in
          state [:application :nicknames]
          assoc pubkey (get (json/read-str content) "name" "tilt"))
      3 (do (printf "%s: %s %s %s\n" kind (article/format-time created_at) (name-of pubkey) content)
            state)
      1 (process-text-event state inner-event)
      4 (do (printf "%s: %s %s %s\n" kind (article/format-time created_at) (name-of pubkey) content)
            state)
      (do (prn "unknown event: " event)
          state)
      )))

(defn process-text-event [state event]
  (let [id (hex-string->num (get event "id"))
        pubkey (hex-string->num (get event "pubkey"))
        sig (hex-string->num (get event "sig"))
        new-event {:id id
                   :pubkey pubkey
                   :created-at (get event "created_at")
                   :content (get event "content")
                   :sig sig
                   :tags (get event "tags")}
        state (assoc-in state [:application :text-event-map id] new-event)
        state (update-in state [:application :chronological-text-events] conj id)]
    state
    ))