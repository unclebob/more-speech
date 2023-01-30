(ns more-speech.ui.swing.tabs-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.swing.tabs :refer :all]
            [more-speech.ui.swing.util :refer :all]
            [more-speech.mem :refer :all]))

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
      (set-mem :tabs-list [{:name "tab0"} {:name "tab1"}])
      (should= 0 (get-tab-index "tab0"))
      (should= 1 (get-tab-index "tab1"))
      (should-be-nil (get-tab-index "tab2"))))

  (context "change-tabs-list-name"
    (it "changes tabs-list name if it exists"
      (set-mem :tabs-list [{:name "old-name"} {:name "some-name"}])
      (change-tabs-list-name "old-name" "new-name")
      (should= [{:name "new-name"} {:name "some-name"}]
               (get-mem :tabs-list)))

    (it "changes does not change tabs-list name if it does not exist"
      (set-mem :tabs-list [{:name "old-name"} {:name "some-name"}])
      (change-tabs-list-name "bad-name" "new-name")
      (should= [{:name "old-name"} {:name "some-name"}]
               (get-mem :tabs-list))))

  (context "delete-tab-from-tabs-list"
    (it "deletes an existing tab"
      (set-mem :tabs-list [{:name "old-name"} {:name "some-name"}])
      (delete-tab-from-tabs-list "old-name")
      (should= [{:name "some-name"}]
               (get-mem :tabs-list))))

  (context "add-tab-to-tabs-list"
    (it "adds-a-tab"
      (set-mem :tabs-list [{:name "some-name"}])
      (add-tab-to-tabs-list "new-name")
      (should= [{:name "some-name"}
                {:name "new-name" :selected [] :blocked []}]
               (get-mem :tabs-list)))
    )

  (context "select-id-in-tab"
    (it "adds-a-tab"
      (set-mem :tabs-list [{:name "tab"} {:name "another"}])
      (add-id-to-tab "tab" :selected 1)
      (should= [{:name "tab" :selected [1]}
                {:name "another"}]
               (get-mem :tabs-list))))
  )
