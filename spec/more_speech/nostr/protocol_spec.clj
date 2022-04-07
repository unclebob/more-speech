(ns more-speech.nostr.protocol-spec
  (:require [speclj.core :refer :all]
            [more-speech.nostr.protocol :refer :all]
            [more-speech.nostr.elliptic-signature :as ecc]))

(describe "aleph-trial"
  (it "builds and signs a text message"
    (let [text (make-text "hello" (ecc/sha-256 (.getBytes "My Private ID.")))
          [message-type {:strs [id pubkey created_at kind tags content sig]}] text]
      (should= "EVENT" message-type)
      (should= 64 (count id))
      (should= 64 (count pubkey))
      (should (number? (eval created_at)))
      (should= 1 kind)
      (should= [] tags)
      (should= "hello" content)
      (should= 128 (count sig))
      (should (ecc/verify (ecc/hex-string->bytes pubkey)
                          (ecc/hex-string->bytes id)
                          (ecc/hex-string->bytes sig)))
      ))
  )

