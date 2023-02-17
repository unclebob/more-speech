(ns more-speech.ui.swing.tabs-spec
  (:use [seesaw core font tree])
  (:require [speclj.core :refer :all]
            [more-speech.ui.swing.tabs :refer :all]
            [more-speech.ui.swing.util :refer :all]
            [more-speech.mem :refer :all]
            [more-speech.nostr.util :as util :refer [hexify]]
            [more-speech.db.in-memory :as in-memory]
            [more-speech.config :as config]
            [more-speech.ui.swing.article-panel :as article-panel]
            [more-speech.db.gateway :as gateway]
            [more-speech.ui.swing.util :as swing-util]
            [more-speech.ui.swing.article-tree :as article-tree])
  (:import (javax.swing.tree DefaultMutableTreeNode)))
(declare db)

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

  (context "filtering events in tabs."
      (it "allows all if the filters are empty"
        (let [event {:id 1}
              filter {:selected []
                      :blocked []}]
          (should (should-add-event? filter event))))

      (it "allows selected event ids"
        (let [events [{:id 1} {:id 2} {:id 3}]
              filter {:selected [1 3]
                      :blocked []}
              filter-results (map #(boolean (should-add-event? filter %)) events)]
          (should= [true false true] filter-results)))

      (it "allows selected pubkeys"
        (let [events [{:id 1 :pubkey 10} {:id 2 :pubkey 20} {:id 3 :pubkey 30}]
              filter {:selected [10 30]
                      :blocked []}
              filter-results (map #(boolean (should-add-event? filter %)) events)]
          (should= [true false true] filter-results)))

      (it "allows selected pubkeys when mentioned in p tags"
        (let [events [{:id 1 :pubkey 10}
                      {:id 2 :pubkey 20 :tags [[:p (util/hexify 50)]]}
                      {:id 3 :pubkey 30}]
              filter {:selected [50]
                      :blocked []}
              filter-results (map #(boolean (should-add-event? filter %)) events)]
          (should= [false true false] filter-results)))

      (it "allows events that have a selected id at the root of a thread."
        (let [events [{:id 1 :tags [[:e "10" ""]]}
                      {:id 2 :tags [[:e "11" ""]]}
                      {:id 3 :tags [[:e "30" ""]]}
                      {:id 4 :tags [[:e "90" ""] [:e "30" ""]]}
                      {:id 5 :tags [[:e "90" ""] [:e "50" ""] [:e "30" ""]]}
                      {:id 6 :tags [[:e "90" ""] [:e "30" ""] [:e "50" ""]]}]
              filter {:selected [16r10 16r30]
                      :blocked []}
              filter-results (map #(boolean (should-add-event? filter %)) events)]
          (should= [true false true false false false] filter-results)))

      (it "does not allow an id or pubkey that is blocked, even if it is selected."
        (let [events [{:id 1 :pubkey 20}
                      {:id 2 :pubkey 20}
                      {:id 3 :pubkey 20}
                      {:id 4 :pubkey 10}]
              filter {:selected [1 2 3 4]
                      :blocked [2 10]}
              filter-results (map #(boolean (should-add-event? filter %)) events)]
          (should= [true false true false] filter-results))
        )
      )

  (context "selecting nodes"
      (with-stubs)
      (with db (in-memory/get-db))
      (before-all (config/set-db! :in-memory))
      (before (in-memory/clear-db @db))

      (it "remembers which articles have been read and loads article"
        (with-redefs
          [article-panel/load-article-info
           (stub :load-article-info {:return nil})]

          (let [selected-event-id 1
                _ (set-mem :back-count 1)
                _ (gateway/add-event @db {:id selected-event-id :content "event"})
                selected-node (DefaultMutableTreeNode. selected-event-id)
                tab-index 0
                _ (select-article tab-index selected-node)
                selected-event (get-mem :selected-event)
                event-history (get-mem :event-history)
                back-count (get-mem :back-count)
                event (gateway/get-event @db selected-event-id)]
            (should (:read event))
            (should-have-invoked :load-article-info {:with [selected-event-id]})
            (should= selected-event selected-event-id)
            (should= [[tab-index selected-event-id]] event-history)
            (should= 0 back-count)))))
  )

(describe "adding ids to tabs"
  (with-stubs)
  (with db (in-memory/get-db))
  (before-all (config/set-db! :in-memory))
  (before (in-memory/clear-db @db))

  (it "adds an an unrooted article id to a tab"
    (gateway/add-event @db {:id 1 :tags []})
    (with-redefs [swing-util/add-id-to-tab (stub :add-id-to-tab)
                  swing-util/relaunch (stub :relaunch)]
      (add-article-to-tab 1 "tab" nil)
      (should-have-invoked :relaunch)
      (should-have-invoked :add-id-to-tab {:with ["tab" :selected 1]})))

  (it "adds the root id of a thread to a tab"
    (let [root-id 100
          event {:id 1 :tags [[:e (util/hexify root-id) "" "root"]]}]
      (gateway/add-event @db event)
      (with-redefs [swing-util/add-id-to-tab (stub :add-id-to-tab)
                    swing-util/relaunch (stub :relaunch)]
        (add-article-to-tab 1 "tab" nil)
        (should-have-invoked :relaunch)
        (should-have-invoked :add-id-to-tab {:with ["tab" :selected root-id]}))))
  )

(defn depict-node [node]
  (loop [ns (range (.getChildCount node))
         node-depiction [(.getUserObject node)]]
    (if (empty? ns)
      node-depiction
      (let [n (first ns)
            child (.getChildAt node n)]
        (recur (rest ns) (conj node-depiction (depict-node child)))))))

(defn depict-tree [tree]
  (let [model (config tree :model)
        root (.getRoot model)]
    (depict-node root)))

(defn add-events [db event-list]
  (doseq [event event-list]
    (gateway/add-event db event)
    (article-tree/add-event event)))

(defn events->tree [db event-list]
  (with-redefs [render-event (stub :render-event)]
    (let [tab-id :tab
          tab {:name tab-id
               :selected []
               :blocked []}
          header-tree (make-header-tree tab-id)
          _ (config! header-tree :id :0 :user-data 0)
          frame (frame :content header-tree)]
      (set-mem :frame frame)
      (set-mem :tabs-list [tab])
      (add-events db event-list)
      (depict-tree header-tree))))

(describe "adding events"
  (with-stubs)
  (with db (in-memory/get-db))
  (before-all (config/set-db! :in-memory))
  (before (in-memory/clear-db @db)
          (clear-mem))

  (it "adds one event"
    (should= [0 [99]]
             (events->tree @db
                           [{:id 99}])))

  (it "adds two events"
    (should= [0 [99] [88]]
             (events->tree @db
                           [{:id 99 :created-at 2}
                            {:id 88 :created-at 1}])))

  (it "adds four events and keeps them in reverse chronological order"
    (should= [0 [66] [77] [88] [99]]
             (events->tree @db
                           [{:id 99 :created-at 1}
                            {:id 88 :created-at 2}
                            {:id 77 :created-at 3}
                            {:id 66 :created-at 4}])))

  (it "adds an event, and a reply when received in order."
    (should= [0 [88] [99 [88]]]
             (events->tree @db
                           [{:id 99 :created-at 1}
                            {:id 88 :created-at 2 :tags [[:e (hexify 99) "" "reply"]]}])))

  (it "adds an event, and a reply when received out of order."
    (should= [0 [88] [99 [88]]]
             (events->tree @db
                           [{:id 88 :created-at 2 :tags [[:e (hexify 99) "" "reply"]]}
                            {:id 99 :created-at 1}])))

  (it "adds a complex chain of replies in order."
    (should= [0 [55] [66] [77 [66]] [88 [77 [66]] [55]] [99 [88 [77 [66]] [55]]]]
             (events->tree @db
                           [{:id 99 :created-at 1}
                            {:id 88 :created-at 2 :tags [[:e (hexify 99) "" "reply"]]}
                            {:id 77 :created-at 3 :tags [[:e (hexify 88) "" "reply"]]}
                            {:id 66 :created-at 4 :tags [[:e (hexify 77) "" "reply"]]}
                            {:id 55 :created-at 5 :tags [[:e (hexify 88) "" "reply"]]}])))

  (it "adds a chain of three replies in order."
    (should= [0 [77] [88 [77]] [99 [88 [77]]]]
             (events->tree @db
                           [{:id 99 :created-at 1}
                            {:id 88 :created-at 2 :tags [[:e (hexify 99) "" "reply"]]}
                            {:id 77 :created-at 3 :tags [[:e (hexify 88) "" "reply"]]}])))

  (it "adds a chain of three replies in reverse order."
    (should= [0 [77] [88 [77]] [99 [88 [77]]]]
             (events->tree @db
                           (reverse
                             [{:id 99 :created-at 1}
                              {:id 88 :created-at 2 :tags [[:e (hexify 99) "" "reply"]]}
                              {:id 77 :created-at 3 :tags [[:e (hexify 88) "" "reply"]]}]))))

  (it "adds a complex chain of replies in reverse order."
    (should= [0 [55] [66] [77 [66]] [88 [55] [77 [66]]] [99 [88 [55] [77 [66]]]]]
             (events->tree @db
                           (reverse
                             [{:id 99 :created-at 1}
                              {:id 88 :created-at 2 :tags [[:e (hexify 99) "" "reply"]]}
                              {:id 77 :created-at 3 :tags [[:e (hexify 88) "" "reply"]]}
                              {:id 66 :created-at 4 :tags [[:e (hexify 77) "" "reply"]]}
                              {:id 55 :created-at 5 :tags [[:e (hexify 88) "" "reply"]]}]))))
  )

(describe "copy-node copies a node and all its children"
  (it "copies one node"
    (let [node (DefaultMutableTreeNode. 1)
          copied-node (article-tree/copy-node node)]
      (should= [1] (depict-node copied-node))))

  (it "copies a node with one child"
    (let [node (DefaultMutableTreeNode. 1)
          child (DefaultMutableTreeNode. 2)
          _ (.add node child)
          copied-node (article-tree/copy-node node)]
      (should= [1 [2]] (depict-node copied-node))))

  (it "copies a node with two children"
    (let [node (DefaultMutableTreeNode. 1)
          _ (.add node (DefaultMutableTreeNode. 2))
          _ (.add node (DefaultMutableTreeNode. 3))
          copied-node (article-tree/copy-node node)]
      (should= [1 [2] [3]] (depict-node copied-node))))

  (it "copies a node with two children and grandchildren"
    (let [node (DefaultMutableTreeNode. 1)
          child1 (DefaultMutableTreeNode. 2)
          _ (.add node child1)
          child2 (DefaultMutableTreeNode. 3)
          _ (.add node child2)
          _ (.add child2 (DefaultMutableTreeNode. 4))
          copied-node (article-tree/copy-node node)]
      (should= [1 [2] [3 [4]]] (depict-node copied-node))))
  )
