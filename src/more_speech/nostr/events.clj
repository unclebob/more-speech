(ns more-speech.nostr.events
  (:require [clojure.data.json :as json]
            [more-speech.logger.default :refer [log-pr]]
            [more-speech.config :refer [get-db]]
            [more-speech.mem :refer :all]
            [more-speech.nostr.util :refer :all]
            [more-speech.nostr.util :as util]
            [more-speech.db.gateway :as gateway])
  (:import (java.nio.charset StandardCharsets)))

(def event-agent (agent nil))

(defn to-json [o]
  (json/write-str o :escape-slash false :escape-unicode false))

(defn get-tag [event target]
  (let [tags (get event :tags [])
        targets (filter #(= target (first %)) tags)]
    (map rest targets)))

(defn make-id
  "returns byte array of id given the clojure form of the body"
  [{:keys [pubkey created_at kind tags content]}]
  (let [id-event (to-json [0 pubkey created_at kind tags content])
        id (util/sha-256 (.getBytes id-event StandardCharsets/UTF_8))]
    id)
  )

(defn- pow2 [n] (reduce * (repeat n 2N)))

(defn make-id-with-pow
  "returns byte array and updated body of id given the clojure form of the body, and the
  POW constraint given in the number of preceding binary zeroes."
  [pow body]
  (let [limit (pow2 (- 256 pow))]
    (loop [nonce 0]
      (let [body (update-in body [:tags] concat [[:nonce (str nonce) (str pow)]])
            id (make-id body)
            id-num (bytes->num id)]
        (if (< id-num limit)
          [id body]
          (recur (inc nonce)))))))

(defn get-unmarked-references [e-tags]
  (let [refs (map second e-tags)
        refs (map hex-string->num refs)
        root (if (empty? refs) nil (first refs))
        referent (last refs)
        mentions (drop-last (rest refs))]
    [root mentions referent])
  )

(defn get-marked-references [e-tags]
  (loop [tags e-tags
         root nil
         referent nil
         mentions []]
    (if (empty? tags)
      (if (nil? root)
        [referent mentions referent]
        [root mentions referent])
      (let [tag (first tags)
            id (hex-string->num (nth tag 1))]
        (if (>= (count tag) 4)
          (condp = (nth tag 3)
            "reply" (recur (rest tags) root id mentions)
            "root" (recur (rest tags) id referent mentions)
            (recur (rest tags) root referent (conj mentions id)))
          (recur (rest tags) root referent (conj mentions id)))))))

(defn get-references
  "returns [root mentions referent] as BigIntegers.
  root is the root id of the thread.
  referent is the id of the event being replied to.
  mentions is a list of cited ids.
  (See NIP-10)"
  [event]
  (try
    (let [tags (:tags event)
          e-tags (filter #(= :e (first %)) tags)
          markers (set (map #(nth % 3) (filter #(>= (count %) 4) e-tags)))]
      (if (contains? markers "reply")
        (get-marked-references e-tags)
        (get-unmarked-references e-tags)))
    (catch Exception e
      (log-pr 1 'get-references 'bad-tags-in-event (.getMessage e) event)
      [nil nil nil])))

;--------called externally by article-tree

(defn get-root-of-thread [id]
  (let [event (gateway/get-event (get-db) id)
        [root _ _] (get-references event)]
    (if (some? root) root id)))

(defn update-event-history [item]
  (update-mem :event-history conj item))

(defn select-event [tab-index id]
  (set-mem :selected-tab tab-index)
  (if-not (get-mem :backing-up)
    (do
      (gateway/update-event-as-read (get-db) id)
      (update-event-history [tab-index id])
      (set-mem :selected-event id)
      (set-mem :back-count 0)
      )
    (do
      (set-mem :selected-event id)
      (set-mem :backing-up false))))
