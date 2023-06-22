(ns more-speech.types.event-counter
  (:require [clojure.spec.alpha :as s]))

(s/def ::relay-url #(re-matches #"ws+://[\w.-]+/?" %))
(s/def ::total int?)
(s/def ::dups int?)
(s/def ::kinds (s/map-of int? int?))

;Counts incoming events and sorts them by relay and kind.  Also counts dups.
(s/def ::event-counter (s/and
                         (s/keys :opt-un [::total
                                        ::dups
                                        ::kinds])
                         (s/map-of (s/or :keyword #{:total :dups :kinds}
                                          :url ::relay-url)
                                   some?)))
