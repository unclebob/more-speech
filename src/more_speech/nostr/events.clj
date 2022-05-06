(ns more-speech.nostr.events
  (:require [clojure.spec.alpha :as s]
            [clojure.data.json :as json]
            [more-speech.nostr.util :refer [hex-string->num]]
            [more-speech.nostr.elliptic-signature :as ecc]
            [clojure.core.async :as async]
            [more-speech.nostr.util :as util])
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
(defn hexify [n]
  (util/num32->hex-string n))

(defprotocol event-handler
  (handle-text-event [handler event])
  )

(declare process-text-event
         process-name-event)

(defn make-event-agent [keys send-chan]
  (agent {:chronological-text-events []
          :nicknames {}
          :text-event-map {}
          :keys keys
          :send-chan send-chan}))

(defn to-json [o]
  (json/write-str o :escape-slash false :escape-unicode false))

(defn process-event [{:keys [nicknames] :as event-state} event]
  (let [_name-of (fn [pubkey] (get nicknames pubkey pubkey))
        [_name _subscription-id inner-event :as _decoded-msg] event
        {:strs [id pubkey _created_at kind _tags _content sig]} inner-event
        valid? (ecc/do-verify (util/hex-string->bytes id)
                              (util/hex-string->bytes pubkey)
                              (util/hex-string->bytes sig))]
    (if (not valid?)
      (do
        (prn 'signature-verification-failed event)
        event-state)
      (condp = kind
        0 (process-name-event event-state inner-event)
        3 (do
            ;(printf "%s: %s %s %s\n" kind (f/format-time created_at) (name-of pubkey) content)
            event-state)
        1 (do
            ;(printf "%s: %s %s %s\n" kind (f/format-time created_at) (name-of pubkey) (subs content 0 (min 50 (count content))))
            (process-text-event event-state inner-event))
        4 (do
            ;(printf "%s: %s %s %s\n" kind (f/format-time created_at) (name-of pubkey) content)
            event-state)
        (do (prn "unknown event: " event)
            event-state)))))

(defn process-name-event [event-state {:strs [_id pubkey _created_at _kind _tags content _sig] :as event}]
  (try
    (let [pubkey (hex-string->num pubkey)
          name (get (json/read-str content) "name" "tilt")]
      (-> event-state
          (update-in [:nicknames] assoc pubkey name)
          ))
    (catch Exception e
      (prn 'json-exception-process-name-event-ignored e)
      (prn event)
      event-state)))

(defn process-tag [[type arg1 arg2]]
  [(keyword type) arg1 arg2])

(defn process-tags [tags]
  (map process-tag tags))

(defn get-references
  "returns [root mentions referent] as BigIntegers"
  [event]
  (let [tags (:tags event)
        e-tags (filter #(= :e (first %)) tags)
        refs (map second e-tags)
        refs (map hex-string->num refs)
        root (if (< (count refs) 2) nil (first refs))
        referent (last refs)
        mentions (drop-last (rest refs))]
    [root mentions referent]))

(defn process-references [state event]
  (let [[_ _ referent] (get-references event)]
    (if (nil? referent)
      state
      (let [referent-path [:text-event-map referent]]
        (if (nil? (get-in state referent-path))
          state
          (update-in
            state
            (concat referent-path [:references])
            conj (:id event)))))))

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

(defn add-event [event-state event]
  (let [id (:id event)
        time (:created-at event)]
    (-> event-state
        (assoc-in [:text-event-map id] event)
        (update-in [:chronological-text-events] conj [id time]))))

(defn process-text-event [event-state event]
  (let [event (translate-text-event event)
        handler (:event-handler event-state)
        event-state (-> event-state
                        (add-event event)
                        (process-references event))]
    (handle-text-event handler event)
    event-state)
  )

(defn chronological-event-comparator [[i1 t1] [i2 t2]]
  (if (= i1 i2)
    0
    (compare t2 t1)))

(defn make-chronological-text-events []
  (sorted-set-by chronological-event-comparator))

(defn make-id
  "returns byte array of id given the clojure form of the body"
  [{:keys [pubkey created_at kind tags content]}]
  (let [id-event (to-json [0 pubkey created_at kind tags content])
        id (util/sha-256 (.getBytes id-event StandardCharsets/UTF_8))]
    id)
  )

(declare make-event-reference-tags
         make-people-reference-tags
         get-reply-root)

(defn compose-text-event
  ([event-state text]
   (compose-text-event event-state text nil))

  ([event-state text reply-to-or-nil]
   (let [private-key (get-in event-state [:keys :private-key])
         private-key (util/hex-string->bytes private-key)
         pubkey (ecc/get-pub-key private-key)
         root (get-reply-root event-state reply-to-or-nil)
         tags (concat (make-event-reference-tags reply-to-or-nil root)
                      (make-people-reference-tags event-state pubkey reply-to-or-nil))
         content text
         now (quot (System/currentTimeMillis) 1000)
         body {:pubkey (util/bytes->hex-string pubkey)
               :created_at now
               :kind 1
               :tags tags
               :content content}
         id (make-id body)
         aux-rand (util/num->bytes 32 (biginteger (System/currentTimeMillis)))
         signature (ecc/do-sign id private-key aux-rand)
         event (assoc body :id (util/bytes->hex-string id)
                           :sig (util/bytes->hex-string signature))
         ]
     ["EVENT" event])))

(defn get-reply-root [event-state reply-to-or-nil]
  (if (nil? reply-to-or-nil)
    nil
    (loop [parent-id reply-to-or-nil
           event-map (:text-event-map event-state)]
      (let [event (get event-map parent-id)
            [_ _ referent] (get-references event)]
        (if (nil? referent)
          parent-id
          (recur referent event-map)))))
  )

(defn make-event-reference-tags
  ([reply-to root]
   (if (or (nil? root) (= root reply-to))
     (make-event-reference-tags reply-to)
     [[:e (hexify root)] [:e (hexify reply-to)]]))

  ([reply-to]
   (if (nil? reply-to)
     []
     [[:e (hexify reply-to)]])
   )
  )

(defn make-people-reference-tags [event-state pubkey reply-to-or-nil]
  (if (nil? reply-to-or-nil)
    []
    (let [event-map (:text-event-map event-state)
          parent-event-id reply-to-or-nil
          parent-event (get event-map parent-event-id)
          parent-tags (:tags parent-event)
          people-ids (map second (filter #(= :p (first %)) parent-tags))
          parent-author (:pubkey parent-event)
          people-ids (conj people-ids (hexify parent-author))
          people-ids (remove #(= (hexify pubkey) %) people-ids)]
      (map #(vector :p %) people-ids))))

(defn send-msg [event-state source-event-or-nil message]
  (let [reply-to-or-nil (:id source-event-or-nil)
        outgoing-event (compose-text-event event-state message reply-to-or-nil)
        send-chan (:send-chan event-state)]
    (async/>!! send-chan [:event outgoing-event])))