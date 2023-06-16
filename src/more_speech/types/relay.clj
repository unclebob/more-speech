(ns more-speech.types.relay
  (:require [clojure.spec.alpha :as s]))

(defn- connection? [c]
  (= (:more-speech.relay/type c) :more-speech.websocket-relay/websocket))

(s/def ::read #{:read-all :read-trusted :read-web-of-trust :read-none})
(s/def ::write boolean?)
(s/def ::retries integer?)
(s/def ::retrying boolean?)
(s/def ::subscribed boolean?)
(s/def ::connection (s/nilable connection?))
(s/def ::relay (s/keys :req-un [::read ::write ]
                       :req-opt [::connection ::subscribed ::retries ::retrying]))
(s/def ::relays (s/map-of string? ::relay))
