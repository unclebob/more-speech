(ns more-speech.ui.formatters-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.formatters :refer :all]))

(describe "reformat article body to fit width."
  (it "should not wrap nothing"
    (should= "" (reformat-article "" 1)))

  (it "should not wrap something shorter than the width."
    (should= "xxx" (reformat-article "xxx" 10)))

  (it "should wrap something longer than the width."
    (should= "xx\nxx" (reformat-article "xxxx" 2)))

  (it "should repeatedly wrap long strings."
    (should= "xx\nxx\nxx" (reformat-article "xxxxxx" 2)))

  (it "should break spaces"
    (should= "x\nx" (reformat-article "x x" 1))
    (should= "x\nx" (reformat-article "x x" 2))
    (should= "xx\nxx" (reformat-article "xx xx" 4)))

  (it "should ignore existing single line breaks."
    (should= "x x" (reformat-article "x\nx" 5)))

  (it "should preserve existing double line breaks."
    (should= "x\n\nx" (reformat-article "x\n\nx" 7)))

  (it "should preserve leading spaces."
    (should= "x\n xxx" (reformat-article "x\n xxx" 6)))

  (it "should give priority to leading spaces"
    (should= "x\n x\n\nx" (reformat-article "x\n x\n\nx" 20)))

  (it "should not break this line. (bug-fix)"
    (should= "I found you!" (reformat-article "I found you!" 50)))
  )
