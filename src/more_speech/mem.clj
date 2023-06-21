(ns more-speech.mem
  (:require [clojure.spec.alpha :as s]
            [more-speech.types.relay :as relay-type]
            [more-speech.types.profile :as profile-type]
            [more-speech.types.active-subscription :as subscription-type]
            [more-speech.types.tab :as tab-type])
  (:import (javax.swing JFrame)))

;(s/def ::id number?)
;(s/def ::created-at number?)
;(s/def ::content string?)
;(s/def ::sig number?)
;(s/def ::tag (s/tuple keyword? number?))
;(s/def ::tags (s/coll-of ::tag))
;(s/def ::references (s/coll-of number?))
;(s/def ::relay-url string?)
;(s/def ::relays (s/coll-of ::relay-url))
;(s/def ::event (s/keys :req-un [::id
;                                ::pubkey
;                                ::created-at
;                                ::content
;                                ::sig
;                                ::tags
;                                ::references]
;                       :opt-un [::relays]))
;
;(s/def ::text-event-map (s/map-of :id :event))
;

;(s/def ::profiles (s/map-of ::id ::profile))
;(s/def ::public-key string?)
;(s/def ::private-key string?)
;(s/def ::keys (s/keys :req-un [::name ::about ::picture ::public-key ::private-key]))
;(s/def ::selected-event ::id)
;(s/def ::event-history (s/coll-of (s/tuple number? ::id)))
;(s/def ::back-count number?)
;(s/def ::backing-up boolean?)
;
;
;(s/def ::id number?)
;(s/def ::orphaned-references (s/map-of ::id (s/coll-of ::id :kind set?)))

(s/def ::pubkey number?) ;The public key of the user
(s/def ::keys ::profile-type/profile)
(s/def ::request-hours-ago int?) ;command line argument
(s/def ::websocket-backlog int?) ;number of unprocessed events
(s/def ::frame #(instance? JFrame %)) ;The main frame
(s/def ::selected-event number?) ;The id of the currently selected event
(s/def ::selected-tab int?) ;index of the selected tab within :tabs-list

(s/def ::mem (s/keys :req-un [::relay-type/relays
                              ::pubkey
                              ::keys]
                     :opt-un [::request-hours-ago
                              ::websocket-backlog
                              ::subscription-type/active-subscriptions
                              ::tab-type/tabs-list
                              ::tab-type/tab-tree-map
                              ::frame
                              ::selected-event
                              ::selected-tab
                              ::node-map
                              ::days-changed
                              ::earliest-loaded-time
                              ::user-configuration
                              ::orphaned-replies
                              ::backing-up
                              ::event-history
                              ::event-counter
                              ::back-count
                              ::send-chan
                              ::article-window
                              ::article-panel
                              ::refresh-main-window
                              ::relay-manager-frame
                              ::tabs-window
                              ::user-window
                              ::event-handler]))

(def memory (atom nil))

(defn conform-mem []
  (when-let [error (s/explain-data ::mem @memory)]
    (prn 'conform-mem error)))

(defn get-mem
  ([]
   memory)

  ([field]
   (if (coll? field)
     (get-in @(get-mem) field)
     (get @(get-mem) field))))

(defn set-mem [key value]
  (if (coll? key)
    (swap! (get-mem) assoc-in key value)
    (swap! (get-mem) assoc key value)))

(defn do-update [map key args]
  (apply (partial update map key) args))

(defn do-update-in [map key args]
  (apply (partial update-in map key) args))

(defn update-mem [key & args]
  (if (coll? key)
    (swap! (get-mem) do-update-in key args)
    (swap! (get-mem) do-update key args))
  (get-mem key))

(defn clear-mem []
  (reset! (get-mem) {}))
