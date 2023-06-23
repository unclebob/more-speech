(ns more-speech.types.event
  (:require [clojure.spec.alpha :as s]))

(s/def ::time number?)
(s/def ::id number?)
(s/def ::created-at ::time)
(s/def ::content string?)
(s/def ::sig number?)
(s/def ::tag (s/cat :tag-name keyword? :tag-data (s/* some?)))
(s/def ::tags (s/coll-of ::tag))
(s/def ::references (s/coll-of ::id))
(s/def ::relay-url string?)
(s/def ::relays (s/coll-of ::relay-url))
(s/def ::event (s/keys :req-un [::id
                                ::pubkey
                                ::created-at
                                ::content
                                ::sig
                                ::tags]
                       :opt-un [::relays
                                ::references]))
