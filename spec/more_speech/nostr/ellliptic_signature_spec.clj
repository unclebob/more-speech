(ns more-speech.nostr.ellliptic-signature-spec
  (:require [speclj.core :refer :all]
            [more-speech.nostr.elliptic-signature :refer :all])
  (:import (java.nio.charset StandardCharsets)))

(describe "verification"
  (it "decodes chars"
    (should= 16rab (Integer/parseInt "ab" 16)))

  (it "does hex-decode"
    (should= "deadbeef" (bytes->hex-string (hex-string->bytes "deadbeef")))
    (should= "aff9a9f017f32b2e8b60754a4102db9d9cf9ff2b967804b50e070780aa45c9a8"
             (bytes->hex-string (hex-string->bytes "aff9a9f017f32b2e8b60754a4102db9d9cf9ff2b967804b50e070780aa45c9a8")))
    )

  (it "converts byte arrays to strings"
    (should= "abcdef" (bytes->hex-string (hex-string->bytes "abcdef"))))

  (it "xors bytes"
    (should= "b2ce" (bytes->hex-string (xor-bytes (hex-string->bytes "af01")
                                                  (hex-string->bytes "1dcf"))))
    (should= "000000000000"
             (bytes->hex-string
               (xor-bytes (hex-string->bytes "deadbeeffeed")
                          (hex-string->bytes "deadbeeffeed"))))
    (should= "ffffffffffff"
             (bytes->hex-string
               (xor-bytes (hex-string->bytes "aaaacccc3333")
                          (hex-string->bytes "55553333cccc"))))
    )

  (it "should convert numbers to bytes"
    (should= "1fcde563"
             (bytes->hex-string
               (num->bytes 4
                           (bytes->num
                             (hex-string->bytes "1fcde563")))))

    (should= "ffcde563"
             (bytes->hex-string
               (num->bytes 4
                           (bytes->num
                             (hex-string->bytes "ffcde563")))))
    )

  (it "signs"
    (let [private-key-1 (sha-256 (.getBytes "my private key 1" StandardCharsets/UTF_8))
          private-key-2 (sha-256 (.getBytes "my private key 2" StandardCharsets/UTF_8))
          public-key-1 (pub-key private-key-1)
          public-key-2 (pub-key private-key-2)
          message-1 (sha-256 (.getBytes "my message 1" StandardCharsets/UTF_8))
          message-2 (sha-256 (.getBytes "my message 2" StandardCharsets/UTF_8))
          sig-1 (sign private-key-1 message-1)
          sig-2 (sign private-key-2 message-2)
          ]
      (should= true (verify public-key-1 message-1 sig-1))
      (should= true (verify public-key-2 message-2 sig-2))
      (should-not (verify public-key-1 message-1 sig-2))
      (should-not (verify public-key-2 message-2 sig-1))
      (should-not (verify public-key-1 message-2 sig-1))
      (should-not (verify public-key-2 message-1 sig-2)))))

