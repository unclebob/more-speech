(ns more-speech.mem
  (:require [clojure.spec.alpha :as s]
            [more-speech.types.active-subscription :as subscription-type]
            [more-speech.types.profile :as profile-type]
            [more-speech.types.relay :as relay-type]
            [more-speech.types.tab :as tab-type]
            [more-speech.types.event-counter :as event-counter-type]
            [more-speech.types.user-configuration :as user-configuration-type]
            [more-speech.types.event :as event-type]
            [more-speech.types.tabs-window :as tabs-window-type]
            [more-speech.types.user-window :as user-window-type])
  (:import (javax.swing JFrame)
           (javax.swing.tree DefaultMutableTreeNode)))



;
;(s/def ::text-event-map (s/map-of :id :event))
;

;(s/def ::profiles (s/map-of ::id ::profile))
;(s/def ::public-key string?)
;(s/def ::private-key string?)
;(s/def ::keys (s/keys :req-un [::name ::about ::picture ::public-key ::private-key]))
;(s/def ::selected-event ::id)
;(s/def ::back-count number?)
;(s/def ::backing-up boolean?)
;
;

;(s/def ::orphaned-references (s/map-of ::id (s/coll-of ::id :kind set?)))


(s/def ::id number?)
(s/def ::tab-index (s/and int? #(not (neg? %))))            ; non negative integer.
(s/def ::pubkey number?)                                    ;The public key of the user
(s/def ::keys ::profile-type/profile)
(s/def ::request-hours-ago int?)                            ;command line argument
(s/def ::websocket-backlog int?)                            ;number of unprocessed events
(s/def ::frame #(instance? JFrame %))                       ;The main frame
(s/def ::selected-event ::id)                               ;The id of the currently selected event
(s/def ::selected-tab ::tab-index)                          ;index of the selected tab within :tabs-list
(s/def ::send-chan #(= clojure.core.async.impl.channels.ManyToManyChannel (type %)))

;map, by id, of all displayed nodes in the tabs.
(s/def ::node-map (s/map-of ::id (s/coll-of #(instance? DefaultMutableTreeNode %))))

;map, by id -- so far unseen -- holding a collection of all known events that reference that id -- i.e. the orphans.
(s/def ::orphaned-replies (s/map-of ::id (s/coll-of ::id)))

(s/def ::article-window-event-id ::id) ;id of article in the article window.
(s/def ::refresh-main-window boolean?) ;when true, main window refreshes once.
(s/def ::relay-manager-frame #(instance? JFrame %)) ;the frame if the relay manager is up.

;The event handler decoupled through an interface.
;should be #(instance? more-speech.nostr.event-dispatcher/event-handler %)
;but that creates a cycle.
(s/def ::event-handler some?)

(s/def ::article-panel (s/keys :req-un [::event-type/event]))

;list, in order, of the last selected events.  Holds the tab index and the id
(s/def ::event-history (s/coll-of (s/tuple ::tab-index ::id) :kind vector?))

;used for managing the back and forward buttons.
;back-count is the number of steps backwards we are in the event-history.
;backing-up is true if the last event selection was because of going back or forward.
(s/def ::back-count int?)
(s/def ::backing-up boolean?)

;keeps track of which relays a particular event was received from.
;used for detecting duplicate events without a DB hit.
(s/def ::processed-event-ids (s/map-of ::id (s/coll-of ::event-type/relay-url :kind set?)))

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
                              ::orphaned-replies
                              ::event-history
                              ::event-counter-type/event-counter
                              ::user-configuration-type/user-configuration
                              ::send-chan
                              ::article-window-event-id
                              ::refresh-main-window
                              ::relay-manager-frame
                              ::event-handler
                              ::article-panel
                              ::tabs-window-type/tabs-window
                              ::user-window-type/user-window
                              ::back-count
                              ::backing-up
                              ::processed-event-ids
                              ]))

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
