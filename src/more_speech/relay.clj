(ns more-speech.relay
  (:refer-clojure :exclude [send]))

(defmulti open ::type)
(defmulti send (fn [relay _message] (::type relay)))
(defmulti close ::type)

