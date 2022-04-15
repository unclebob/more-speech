(ns more-speech.nostr.events
  (:require [clojure.spec.alpha :as s]
            [clojure.data.json :as json]
            [more-speech.nostr.util :refer [hex-string->num]]
            [more-speech.ui.widget :as w]
            [more-speech.nostr.elliptic-signature :as ecc])
  (:import (java.nio.charset StandardCharsets)))
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
        _name-of (fn [pubkey] (get nicknames pubkey pubkey))
        [_name _subscription-id inner-event :as _decoded-msg] event
        {:strs [_id _pubkey _created_at kind _tags _content _sig]} inner-event]
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
        refs (map hex-string->num (take 1 refs))            ;; Hack.  Only the first reference is counted.
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

(defn add-event [state event]
  (let [id (:id event)
        time (:created-at event)
        state (assoc-in state [:application :text-event-map id] event)
        state (update-in state [:application :chronological-text-events] conj [id time])]
    state))

(defn process-text-event [state event]
  (let [event (translate-text-event event)
        state (add-event state event)
        state (w/redraw-widget state [:application :header-window])]
    (process-references state event)))

(defn chronological-event-comparator [[i1 t1] [i2 t2]]
  (if (= i1 i2)
    0
    (compare t2 t1)))

(defn make-chronological-text-events []
  (sorted-set-by chronological-event-comparator))

(defn make-id
  "returns byte array of id given the clojure form of the body"
  [{:keys [pubkey created_at kind tags content]}]
  (let [id-event (json/write-str [0 pubkey created_at kind tags content])
        id (ecc/sha-256 (.getBytes id-event StandardCharsets/UTF_8))]
    id)
  )

(defn compose-text-event [private-key text]
  (let [private-key (ecc/hex-string->bytes private-key)
        pubkey (ecc/pub-key private-key)
        tags []
        content text
        now (quot (System/currentTimeMillis) 1000)
        body {:pubkey (ecc/bytes->hex-string pubkey)
              :created_at now
              :kind 1
              :tags tags
              :content content}
        id (make-id body)
        aux-rand (ecc/num->bytes 32 (biginteger (System/currentTimeMillis)))
        signature (ecc/do-sign id private-key aux-rand)
        event (assoc body :id (ecc/bytes->hex-string id)
                          :sig (ecc/bytes->hex-string signature))
        ]
    ["EVENT" event]))