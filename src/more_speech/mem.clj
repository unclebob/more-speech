(ns more-speech.mem
  (:require [clojure.spec.alpha :as s]))

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


(s/def ::id number?)
(s/def ::orphaned-references (s/map-of ::id (s/coll-of ::id :kind set?)))

(def memory (atom nil))
(def relays (atom nil)) ;ick.  Should be in memory.

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
