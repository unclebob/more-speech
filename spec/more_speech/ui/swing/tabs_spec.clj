(ns more-speech.ui.swing.tabs-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.swing.tabs :refer :all]))

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
  )