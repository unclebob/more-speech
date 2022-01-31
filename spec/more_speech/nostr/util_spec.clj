(ns more-speech.nostr.util-spec
  (:require [speclj.core :refer :all]
            [more-speech.nostr.util :refer :all]))

(describe "Hex string and bytes conversions"
  (it "does hex-decode"
    (should= "deadbeef" (bytes->hex-string (hex-string->bytes "deadbeef")))
    (should= "aff9a9f017f32b2e8b60754a4102db9d9cf9ff2b967804b50e070780aa45c9a8"
             (bytes->hex-string (hex-string->bytes "aff9a9f017f32b2e8b60754a4102db9d9cf9ff2b967804b50e070780aa45c9a8")))
    )

  (it "converts byte arrays to strings"
    (should= "abcdef" (bytes->hex-string (hex-string->bytes "abcdef"))))

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
  )
