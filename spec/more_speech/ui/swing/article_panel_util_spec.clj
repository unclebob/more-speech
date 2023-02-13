(ns more-speech.ui.swing.article-panel-util-spec
  (:require [speclj.core :refer :all]
            [more-speech.mem :refer :all]
            [more-speech.ui.swing.article-panel-util :refer :all]))

(describe "article-panel-util"
  (before (clear-mem))

  (context "adjust-back-count"
    (it "increments back-count"
      (set-mem :event-history [1 2 3])
      (set-mem :back-count 0)
      (should= 1 (:back-count (adjust-back-count 1))))

    (it "does not go negative."
      (set-mem :event-history [1 2 3])
      (set-mem :back-count 0)
      (should= 0 (:back-count (adjust-back-count -1))))

    (it "does not go beyond end of event history list."
      (set-mem :event-history [1 2 3])
      (set-mem :back-count 2)
      (should= 2 (:back-count (adjust-back-count 1))))
    )

  (context "go-back-by"
    (with-stubs)
    (it "displays the appropriate event and tab"
      (with-redefs [display-event (stub :display-event)]
        (set-mem :event-history [[:t1 1] [:t2 2]])
        (set-mem :back-count 0)
        (go-back-by 1)
        (should-have-invoked :display-event {:with [:t1 1]})))
    )
  )
