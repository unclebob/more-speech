(ns more-speech.nostr.events
  (:require [clojure.spec.alpha :as s]
            [clojure.data.json :as json]
            [more-speech.nostr.util :refer [hex-string->num]]
            [more-speech.ui.widget :as w]
            [more-speech.ui.formatters :as f]
            ))
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
(declare process-text-event
         process-name-event)

(defn process-event [{:keys [application] :as state} event]
  (let [{:keys [_articles nicknames]} application
        name-of (fn [pubkey] (get nicknames pubkey pubkey))
        [_name _subscription-id inner-event :as _decoded-msg] event
        {:strs [_id pubkey created_at kind _tags content _sig]} inner-event]
    (condp = kind
      0 (process-name-event state inner-event)
      3 (do
          ;(printf "%s: %s %s %s\n" kind (f/format-time created_at) (name-of pubkey) content)
            state)
      1 (do
          ;(printf "%s: %s %s %s\n" kind (f/format-time created_at) (name-of pubkey) (subs content 0 (min 50 (count content))))
          (process-text-event state inner-event))
      4 (do
          ;(printf "%s: %s %s %s\n" kind (f/format-time created_at) (name-of pubkey) content)
            state)
      (do (prn "unknown event: " event)
          state))))

(defn process-name-event [state {:strs [_id pubkey _created_at _kind _tags content _sig]}]
  (let [pubkey (hex-string->num pubkey)
        name (get (json/read-str content) "name" "tilt")
        state (w/redraw-widget state [:application :author-window])]
    (update-in
      state [:application :nicknames] assoc pubkey name)))

(defn process-tag [[type arg1 arg2]]
  [(keyword type) arg1 arg2])

(defn process-tags [tags]
  (map process-tag tags))

(defn process-references [state {:keys [id tags] :as _event}]
  (let [e-tags (filter #(= :e (first %)) tags)
        refs (map second e-tags)
        refs (map hex-string->num (take 1 refs)) ;; Hack.  Only the first reference is counted.
        ]
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
                     (concat referent-path [:references])
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

(defn by-event-time [event-map id1 id2]
  (let [event1 (get event-map id1)
        event2 (get event-map id2)]
    (< (get event1 :created-at)
       (get event2 :created-at))))

(defn process-text-event [state event]
  (let [event (translate-text-event event)
        id (:id event)
        state (assoc-in state [:application :text-event-map id] event)
        event-map (get-in state [:application :text-event-map] {})
        events (get-in state [:application :chronological-text-events] [])
        events (conj events id)
        events (sort (partial by-event-time event-map) events)
        state (assoc-in state [:application :chronological-text-events] events)
        ;state (update-in state [:application :chronological-text-events] conj id)
        state (w/redraw-widget state [:application :header-window])
        ]
    (process-references state event)))