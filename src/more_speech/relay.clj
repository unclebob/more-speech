(ns more-speech.relay)

(defmulti open ::type)
(defmulti send (fn [relay _messge] (::type relay)))
(defmulti close ::type)

