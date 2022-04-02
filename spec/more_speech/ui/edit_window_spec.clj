(ns more-speech.ui.edit-window-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.edit-window :refer :all]
            )
  )

(describe "edit-window"
  (context "adding characters"
    (it "creates text field if none exists"
      (let [frame {}
            frame (add-char frame \a)]
        (should= "a" (:text frame))))

    (it "appends to text field if exists."
      (let [frame {:text "a"}
                  frame (add-char frame \a)]
              (should= "aa" (:text frame))))

      ))
