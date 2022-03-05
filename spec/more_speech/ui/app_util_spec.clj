(ns more-speech.ui.app-util-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.app-util :refer :all]))

(declare application
         state
         button)

(describe "select headers"
  (with application {:selected-header nil})
  (with state {:application @application})
  (with button {:id 42})

  (it "selects the article"
    (let [state (select-header 42 @state)]
      (should= 42 (get-in state [:application :selected-header]))))

  (it "deselects the selected article."
    (let [state (select-header @button @state)
          state (select-header @button state)]
          (should= nil (get-in state [:application :selected-header]))))
  )
