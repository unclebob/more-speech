(ns more-speech.nostr.events
  (:require [clojure.spec.alpha :as s]
            [clojure.data.json :as json]
            [more-speech.config :refer [get-db]]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.nostr.util :refer :all]
            [more-speech.nostr.util :as util]
            [more-speech.db.gateway :as gateway])
  (:import (java.nio.charset StandardCharsets)))

(s/def ::id number?)
(s/def ::pubkey number?)
(s/def ::created-at number?)
(s/def ::content string?)
(s/def ::sig number?)
(s/def ::tag (s/tuple keyword? number?))
(s/def ::tags (s/coll-of ::tag))
(s/def ::references (s/coll-of number?))
(s/def ::relay-url string?)
(s/def ::relays (s/coll-of ::relay-url))
(s/def ::event (s/keys :req-un [::id
                                ::pubkey
                                ::created-at
                                ::content
                                ::sig
                                ::tags
                                ::references]
                       :opt-un [::relays]))

(s/def ::text-event-map (s/map-of :id :event))
(s/def ::name string?)
(s/def ::about string?)
(s/def ::picture string?)
(s/def ::profile (s/keys :req-un [::name ::about ::picture]))
(s/def ::profiles (s/map-of ::id ::profile))
(s/def ::public-key string?)
(s/def ::private-key string?)
(s/def ::keys (s/keys :req-un [::name ::about ::picture ::public-key ::private-key]))
(s/def ::selected (s/coll-of ::id))
(s/def ::blocked (s/coll-of ::id))
(s/def ::tab (s/keys :req-un [::name ::selected ::blocked]))
(s/def ::tabs-list (s/coll-of ::tab))
(s/def ::selected-event ::id)
(s/def ::event-history (s/coll-of (s/tuple number? ::id)))
(s/def ::back-count number?)
(s/def ::backing-up boolean?)

(s/def ::event-context (s/keys :req-un [::text-event-map
                                        ::profiles
                                        ::keys
                                        ::tabs-list
                                        ::selected-event
                                        ::event-history
                                        ::back-count
                                        ::backing-up]))

(defn make-event-context [event-context-map]
  (atom (merge {:text-event-map {}
                :profiles {}
                :keys {}
                :tabs-list []
                :event-history []
                :back-count 0
                }
               event-context-map)))

(def event-agent (agent nil))

(defn hexify [n]
  (util/num32->hex-string n))

(defn to-json [o]
  (json/write-str o :escape-slash false :escape-unicode false))

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
      (prn 'get-references 'bad-tags-in-event (.getMessage e) event)
      [nil nil nil])))

;--------called externally by article-tree

(defn get-root-of-thread [id]
  (let [event (gateway/get-event (get-db) id)
        [root _ _] (get-references event)]
    (if (some? root) root id)))

(defn update-event-history [item]
  (set-mem :event-history (conj (get-mem :event-history) item)))

(defn select-event [tab-index id]
  (swap! ui-context assoc :selected-tab tab-index)
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
