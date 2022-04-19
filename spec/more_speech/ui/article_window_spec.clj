(ns more-speech.ui.article-window-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.article-window :refer :all]
            [more-speech.ui.object-mother :as mom]))

(declare state now)
(describe "Reply"
  (with state (mom/make-test-state))
  (with now (quot (System/currentTimeMillis) 1000))

  (it "will not create reply if no article is being displayed"
    (let [state (assoc-in @state [:application :selected-header] nil)
          state (reply-to-article state)
          edit-frame (get-in state [:application :edit-window :text-frame])]
      (should= [""] (get edit-frame :text))
      (should= [0 0] (get edit-frame :insertion-point))))

  (it "will create a simple reply if a simple article is being displayed"
    (let [state (assoc-in @state [:application :selected-header] 1)
          event-map (get-in state [:application :text-event-map])
          event-map (assoc event-map 1 {:id "1"
                                        :pubkey 0xf00d
                                        :created-at @now
                                        :kind 1
                                        :tags []
                                        :content "simple"
                                        :sig 0xdddddd})
          state (assoc-in state [:application :text-event-map] event-map)
          state (reply-to-article state)
          edit-frame (get-in state [:application :edit-window :text-frame])]
      (should= [">simple"] (get edit-frame :text))
      (should= [0 0] (get edit-frame :insertion-point))))
  )
