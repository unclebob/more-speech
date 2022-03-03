(ns more-speech.ui.author-window-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.author-window :refer :all]
            [clojure.spec.alpha :as s]))

(describe "Formatting Utilities"
  (context "Abbreviations"
    (it "abbreviates pubkeys"
      (should= "short" (abbreviate-key "short"))
      (should= "long pub..." (abbreviate-key "long pubkey")))
    )
  )

(describe "Formatting an author nickname."
  (let [author-tuple [0 "nickname"]]
    (it "conforms to spec."
      (should (s/valid? ::author-nickname-tuple author-tuple)))

    (it "is properly formatted."
      (should= [:bold
                "00000000..."
                :regular
                " - "
                "nickname"
                :new-line]
               (markup-author author-tuple)))))