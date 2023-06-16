(ns more-speech.types.profile
  (:require [clojure.spec.alpha :as s]))

(s/def ::public-key #(re-matches #"[0-9a-f]{64}" %))
(s/def ::private-key #(re-matches #"[0-9a-f]{64}" %))
(s/def ::name string?)
(s/def ::about string?)
(s/def ::picture string?)
(s/def ::lud16 string?)
(s/def ::lud06 string?)
(s/def ::banner string?)
(s/def ::display-name string?)
(s/def ::website string?)
(s/def ::created-at number?)
(s/def ::wallet-connect string?)
(s/def ::password string?)
(s/def ::profile (s/keys :req-un [::name
                                  ::about
                                  ::picture
                                  ::public-key
                                  ::private-key]
                         :req-opt [::lud16
                                   ::lud06
                                   ::banner
                                   ::display-name
                                   ::website
                                   ::nip05
                                   ::created-at
                                   ::wallet-connect
                                   ::password]))
