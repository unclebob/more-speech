(ns more-speech.types.zaps
  (:require [clojure.spec.alpha :as s]))

(s/def ::invoice string?)
(s/def ::id number?)
(s/def ::amount pos-int?)
(s/def ::comment string?)

(s/def ::transaction (s/keys :req-un [::id
                                      ::amount
                                      ::comment]))

(s/def ::pending-zaps (s/map-of ::invoice ::transaction))
