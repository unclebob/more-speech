(ns more-speech.types.active-subscription
  (:require [clojure.spec.alpha :as s]))

(s/def ::subscription-url string?)
(s/def ::subscription-id string?)
(s/def ::eose #{:close :next-batch}) ;action to perform on EOSE
(s/def ::min-time int?) ;time of earliest event in this batch
(s/def ::max-time int?) ;time of latest event in this batch
(s/def ::last-batch-min-time int?) ;time of earliest event in last batch
(s/def ::back-to int?) ;batching ends when we get here.
(s/def ::since int?) ;earliest requested time for this batch
(s/def ::filter map?) ; other filters as per REQ
(s/def ::event-counter int?) ;number of events in this batch
(s/def ::batch-closed boolean?) ;batch complete EOSE received.
(s/def ::subscription (s/keys :opt-un [::eose
                                       ::batch-closed
                                       ::min-time
                                       ::max-time
                                       ::event-counter
                                       ::last-batch-min-time
                                       ::back-to
                                       ::since
                                       ::until
                                       ::filter
                                       ]))
(s/def ::subscriptions (s/or :nil nil?
                             :subscriptions (s/map-of ::subscription-id ::subscription)))
(s/def ::active-subscriptions (s/or :nil nil?
                                    :subscriptions (s/map-of ::subscription-url ::subscriptions)))
