(ns more-speech.nostr.elliptic-signature
  (:import (schnorr Schnorr)))

(defn do-sign [message private-key aux-rand]
  (Schnorr/sign message private-key aux-rand))

(defn do-verify [message public-key signature]
  (Schnorr/verify message public-key signature))

(defn get-pub-key
  "private-key is byte array.  Returns byte array."
  [private-key]
  (Schnorr/genPubKey private-key))



