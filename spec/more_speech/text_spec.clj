(ns more-speech.text-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.cursor :refer :all]
            [quil.core :as q]))

(describe "Text Utilities"
  (context "nil->blank"
    (it "protects against nil"
      (should= "" (nil->blank nil))
      (should= "not nil" (nil->blank "not nil")))))

