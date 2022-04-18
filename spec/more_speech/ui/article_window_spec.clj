(ns more-speech.ui.article-window-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.article-window :refer :all]
            [more-speech.ui.object-mother :as mom]))

(describe "Reply"
  (let [state (mom/make-test-state)
        article-window (get-in state [:application :article-window])
        article-frame (get article-window :text-frame)]
    (it "will not create reply if no article is being displayed"
      (let [article-frame (assoc article-frame :displayed-article nil)
            state (assoc-in state (:path article-frame) article-frame)
            state (reply-to-article state article-frame)
            edit-frame (get-in state [:application :edit-window :text-frame])]
        (should= [""] (get edit-frame :text))
        (should= [0 0] (get edit-frame :insertion-point))
        ))
    )
  )
