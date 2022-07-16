(ns more-speech.ui.swing.tabs-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.swing.tabs :refer :all]
            [more-speech.ui.swing.util :refer :all]
            [more-speech.ui.swing.ui-context :refer :all]))

(describe "tabs"
  (context "ensure-tab-list-has-all"
    (it "adds 'all' to tabs that don't have 'all'"
      (let [tab-list [{:name "tab1" :selected [1] :blocked [2]}]]
        (should= [{:name "tab1" :selected [1] :blocked [2]}
                  {:name "all" :selected [] :blocked []}]
                 (ensure-tab-list-has-all tab-list))))

    (it "does not add 'all' if it exists"
      (let [tab-list [{:name "all" :selected [1] :blocked [2]}
                      {:name "tab" :selected [3] :blocked 4}]]
        (should= tab-list (ensure-tab-list-has-all tab-list))
        ))
    )

  (context "get-tab-index"
    (it "gets the index of a tab using the name"
      (let [tab-list [{:name "tab0"} {:name "tab1"}]
            event-context {:tabs-list tab-list}]
        (reset! ui-context {:event-context (atom event-context)})
        (should= 0 (get-tab-index "tab0"))
        (should= 1 (get-tab-index "tab1"))
        (should-be-nil (get-tab-index "tab2")))))
  )