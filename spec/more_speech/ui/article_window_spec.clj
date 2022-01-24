(ns more-speech.ui.article-window-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.article-window :refer :all]
            [more-speech.ui.widget :refer [widget setup-widget]]))

(describe "article window"
  (context "setup"
    (it "has the components"
      (let [state {}
            article-window (map->article-window {:x 0 :y 0 :h 100 :w 100})
            article-window (setup-widget article-window state)]
        (should (satisfies? widget (:page-up article-window)))
        (should (satisfies? widget (:page-down article-window)))
        ))))
