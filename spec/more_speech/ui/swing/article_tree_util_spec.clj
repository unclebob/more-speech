(ns more-speech.ui.swing.article-tree-util-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.ui.swing.article-tree-util :refer :all :as article-tree-util]))

(describe "Article Tree Util"
  (context "adjust-back-count"
    (it "increments back-count"
      (let [event-data {:event-history [1 2 3]
                        :back-count 0}]
        (should= 1 (:back-count (adjust-back-count event-data 1)))))

    (it "does not go negative."
      (let [event-data {:event-history [1 2 3]
                        :back-count 0}]
        (should= 0 (:back-count (adjust-back-count event-data -1)))))

    (it "does not go beyond end of event history list."
      (let [event-data {:event-history [1 2 3]
                        :back-count 2}]
        (should= 2 (:back-count (adjust-back-count event-data 1)))))
    )

  (context "go-back-by"
    (with-stubs)
    (it "displays the appropriate event and tab"
      (with-redefs [article-tree-util/display-event (stub :display-event)]
        (let [event-data {:event-history [[:t1 1]
                                          [:t2 2]]
                          :back-count 0}
              ui-context-data {:event-context (atom event-data)}
              _ (reset! ui-context ui-context-data)]
          (go-back-by 1)
          (should-have-invoked :display-event {:with [:t1 1]}))
        )
      )
    )
  )
