(ns more-speech.data-storage)

(defmulti write-tabs (fn [] :files))
(defmulti write-relays (fn [] :files))
(defmulti write-keys (fn [_keys] :files))
(defmulti write-user-configuration (fn [] :files))