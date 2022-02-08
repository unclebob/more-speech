(ns more-speech.nostr.events
  (:require [clojure.spec.alpha :as s]
            [more-speech.content.article :as article]
            [clojure.data.json :as json]
            [more-speech.nostr.util :refer [hex-string->num]]))
(s/def ::id number?)
(s/def ::pubkey number?)
(s/def ::created-at number?)
(s/def ::content string?)
(s/def ::sig number?)
(s/def ::tag (s/tuple keyword? number?))
(s/def ::tags (s/coll-of ::tag))
(s/def ::references (s/coll-of number?))
(s/def ::event (s/keys :req-un [::id
                                ::pubkey
                                ::created-at
                                ::content
                                ::sig
                                ::tags
                                ::references]))
(declare process-text-event)

(defn process-event [{:keys [application] :as state} event]
  (let [{:keys [articles nicknames]} application
        name-of (fn [pubkey] (get nicknames pubkey pubkey))
        [name subscription-id inner-event :as decoded-msg] event
        {:strs [id pubkey created_at kind tags content sig]} inner-event]
    (condp = kind
      0 (let [pubkey (hex-string->num pubkey)
              name (get (json/read-str content) "name" "tilt")]
          (update-in
            state [:application :nicknames] assoc pubkey name))
      3 (do (printf "%s: %s %s %s\n" kind (article/format-time created_at) (name-of pubkey) content)
            state)
      1 (process-text-event state inner-event)
      4 (do (printf "%s: %s %s %s\n" kind (article/format-time created_at) (name-of pubkey) content)
            state)
      (do (prn "unknown event: " event)
          state))))

(defn process-tag [[type hex arg]]
  [(keyword type)
   (hex-string->num hex)
   arg])

(defn process-tags [tags]
  (map process-tag tags))

(defn process-references [state {:keys [id tags] :as event}]
  (let [e-tags (filter #(= :e (first %)) tags)
        refs (map second e-tags)]
    (loop [refs refs
           state state]
      (if (empty? refs)
        state
        (let [referent-path [:application :text-event-map (first refs)]]
          (if (nil? (get-in state referent-path))
            (recur (rest refs) state)
            (recur (rest refs)
                   (update-in
                     state
                     (conj referent-path :references)
                     conj id))))))))

(defn translate-text-event [event]
  (let [id (hex-string->num (get event "id"))
        pubkey (hex-string->num (get event "pubkey"))
        sig (hex-string->num (get event "sig"))]
    {:id id
     :pubkey pubkey
     :created-at (get event "created_at")
     :content (get event "content")
     :sig sig
     :tags (process-tags (get event "tags"))}))

(defn process-text-event [state event]
  (let [event (translate-text-event event)
        id (:id event)
        state (assoc-in state [:application :text-event-map id] event)
        state (update-in state [:application :chronological-text-events] conj id)]
    (process-references state event)))