(ns more-speech.ui.swing.tabs-spec
  (:require [more-speech.config :as config]
            [more-speech.data-storage :as data-storage]
            [more-speech.db.gateway :as gateway]
            [more-speech.mem :refer :all]
            [more-speech.nostr.util :as util]
            [more-speech.spec-util :refer :all]
            [more-speech.ui.swing.article-panel :as article-panel]
            [more-speech.ui.swing.main-window :as main-window]
            [more-speech.ui.swing.tabs :refer :all]
            [more-speech.ui.swing.util :refer :all]
            [more-speech.ui.swing.util :as swing-util]
            [speclj.core :refer :all])
  (:use (seesaw [core] [font] [tree]))
  (:import (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel)))
(declare db)

(describe "tabs"
  (with-stubs)
  (setup-db-mem)
  (context "ensure-tab-list-has-defaults"
    (it "adds 'all' and 'trusted' to tabs that don't have them."
      (let [tab-list [{:name "tab1" :selected [1] :blocked [2]}]]
        (should= [{:name "tab1" :selected [1] :blocked [2]}
                  {:name "all" :selected [] :blocked []}
                  {:name "trusted" :selected [:trusted] :blocked []}]
                 (ensure-tab-list-has-defaults tab-list))))

    (it "does not add 'all' or 'trusted' if they exists"
      (let [tab-list [{:name "all" :selected [1] :blocked [2]}
                      {:name "tab" :selected [3] :blocked 4}
                      {:name "trusted" :selected [:trusted] :blocked []}]]
        (should= tab-list (ensure-tab-list-has-defaults tab-list))))
    )

  (context "get-tab-index"
    (it "gets the index of a tab using the name"
      (set-mem :tabs-list [{:name "tab0"} {:name "tab1"}])
      (should= 0 (get-tab-index "tab0"))
      (should= 1 (get-tab-index "tab1"))
      (should-be-nil (get-tab-index "tab2"))))

  (context "change-tabs-list-name"
    (redefs-around [data-storage/write-tabs (stub :write-tabs)])

    (it "changes tabs-list name if it exists"
      (set-mem :tabs-list [{:name "old-name"} {:name "some-name"}])
      (change-tabs-list-name "old-name" "new-name")
      (should= [{:name "new-name"} {:name "some-name"}]
               (get-mem :tabs-list))
      (should-have-invoked :write-tabs))

    (it "changes does not change tabs-list name if it does not exist"
      (set-mem :tabs-list [{:name "old-name"} {:name "some-name"}])
      (change-tabs-list-name "bad-name" "new-name")
      (should= [{:name "old-name"} {:name "some-name"}]
               (get-mem :tabs-list))))

  (context "delete-tab-from-tabs-list"
    (redefs-around [data-storage/write-tabs (stub :write-tabs)])

    (it "deletes an existing tab"
      (set-mem :tabs-list [{:name "old-name"} {:name "some-name"}])
      (delete-tab-from-tabs-list "old-name")
      (should-have-invoked :write-tabs)
      (should= [{:name "some-name"}]
               (get-mem :tabs-list))))

  (context "add-tab-to-tabs-list"
    (redefs-around [data-storage/write-tabs (stub :write-tabs)])

    (it "adds-a-tab"
      (set-mem :tabs-list [{:name "some-name"}])
      (add-tab-to-tabs-list "new-name")
      (should-have-invoked :write-tabs)
      (should= [{:name "some-name"}
                {:name "new-name" :selected [:empty] :blocked []}]
               (get-mem :tabs-list)))
    )

  (context "select-id-in-tab"
    (redefs-around [data-storage/write-tabs (stub :write-tabs)])

    (it "adds-a-tab"
      (set-mem :tabs-list [{:name "tab"} {:name "another"}])
      (add-filter-to-tab "tab" :selected 1)
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

    (it "allows trusted authors if :trusted filter"
      (set-mem :pubkey 99)
      (gateway/add-contacts @db 99 [{:pubkey 10}])
      (let [events [{:id 1 :pubkey 10} {:id 2 :pubkey 20} {:id 3 :pubkey 30}]
            filter {:selected [:trusted]
                    :blocked []}
            filter-results (map #(boolean (should-add-event? filter %)) events)]
        (should= [true false false] filter-results)))

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
        (should= [true false true false] filter-results)))

    (it "allows notes whose contents match a string"
      (let [filter {:selected ["alpha" "match" "beta"] :blocked []}
            event {:content "match"}]
        (should (should-add-event? filter event))))

    (it "does not select notes whose contents do not match a string"
      (let [filter {:selected ["alpha" "nope" "beta"] :blocked []}
            event {:content "match"}]
        (should-not (should-add-event? filter event))))

    (it "allows notes whose subjects match a string"
      (let [filter {:selected ["alpha" "match" "beta"] :blocked []}
            event {:content "no" :tags [[:subject "match"]]}]
        (should (should-add-event? filter event))))
    )

  (context "selecting nodes"
    (with-stubs)
    (setup-db-mem)

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
  (redefs-around [data-storage/write-tabs (stub :write-tabs)])
  (setup-db-mem)

  (it "adds an an unrooted article id to a tab"
    (gateway/add-event @db {:id 1 :tags []})
    (with-redefs [swing-util/relaunch (stub :relaunch)
                  swing-util/select-tab (stub :select-tab)
                  add-event-to-tab (stub :add-event-to-tab)]
      (add-tab-to-tabs-list "tab")
      (add-article-to-tab 1 "tab" nil)
      (should-have-invoked :select-tab {:with ["tab"]})
      (should-have-invoked :add-event-to-tab)
      (should= [{:name "tab", :selected [:empty 1], :blocked []}]
               (get-mem :tabs-list))
      (should-have-invoked :write-tabs)))

  (it "adds the root id of a thread to a tab"
    (let [root-id 100
          event {:id 1 :tags [[:e (util/hexify root-id) "" "root"]]}]
      (gateway/add-event @db event)
      (with-redefs [add-event-to-tab (stub :add-event-to-tab)
                    swing-util/select-tab (stub :select-tab)]
        (add-tab-to-tabs-list "tab")
        (add-article-to-tab 1 "tab" nil)
        (should-have-invoked :select-tab {:with ["tab"]})
        (should-have-invoked :add-event-to-tab)
        (should= [{:name "tab", :selected [:empty 100], :blocked []}]
                 (get-mem :tabs-list))
        (should-have-invoked :write-tabs))))
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
    (main-window/add-event event)))

(defn events->tree [db event-list]
  (with-redefs [render-event (stub :render-event)]
    (let [tab-id "tab"
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
  (setup-db-mem)

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
  )

(describe "Pruning tabs"
  (before clear-mem)

  (it "can delete a node"
    (let [root (DefaultMutableTreeNode. 1)
          model (DefaultTreeModel. root)
          node (DefaultMutableTreeNode. 2)]
      (.insertNodeInto model node root 0)
      (should= 1 (.getChildCount root))
      (.removeNodeFromParent model node)
      (should= 0 (.getChildCount root))
      ))

  (it "deletes last node"
    (let [root (DefaultMutableTreeNode. 0)
          model (DefaultTreeModel. root)
          node1 (DefaultMutableTreeNode. 1)
          last-event-id 99
          last-node (DefaultMutableTreeNode. last-event-id)]
      (.insertNodeInto model node1 root 0)
      (.insertNodeInto model last-node root 1)
      (set-mem [:node-map last-event-id] [:dummy last-node])
      (should= 2 (.getChildCount root))
      (delete-last-event-from-tree-model model)
      (should= 1 (.getChildCount root))
      (should= 1 (.getUserObject ^DefaultMutableTreeNode (.getChild model root 0)))
      (should= [:dummy] (get-mem [:node-map last-event-id]))))

  (it "deletes last node and all its children"
    (let [root (DefaultMutableTreeNode. 0)
          model (DefaultTreeModel. root)
          node1 (DefaultMutableTreeNode. 1)
          last-event-id 99
          last-node (DefaultMutableTreeNode. last-event-id)
          child1-id 199
          child2-id 299
          child3-id 399
          child1-node (DefaultMutableTreeNode. child1-id)
          child2-node (DefaultMutableTreeNode. child2-id)
          child3-node (DefaultMutableTreeNode. child3-id)]
      (.add last-node child1-node)
      (.add child1-node child2-node)
      (.add last-node child3-node)
      (.insertNodeInto model node1 root 0)
      (.insertNodeInto model last-node root 1)
      (set-mem [:node-map last-event-id] [:dummy last-node])
      (set-mem [:node-map child1-id] [:dummy child1-node])
      (set-mem [:node-map child2-id] [:dummy child2-node])
      (set-mem [:node-map child3-id] [:dummy child3-node])
      (should= 2 (.getChildCount root))
      (delete-last-event-from-tree-model model)
      (should= 1 (.getChildCount root))
      (should= 1 (.getUserObject ^DefaultMutableTreeNode (.getChild model root 0)))
      (should= [:dummy] (get-mem [:node-map last-event-id]))
      (should= [:dummy] (get-mem [:node-map child1-id]))
      (should= [:dummy] (get-mem [:node-map child2-id]))
      (should= [:dummy] (get-mem [:node-map child3-id]))))

  (it "deletes last nodes if too many"
    (let [root (DefaultMutableTreeNode. 0)
          model (DefaultTreeModel. root)
          node1 (DefaultMutableTreeNode. 1)
          node2 (DefaultMutableTreeNode. 2)
          node3 (DefaultMutableTreeNode. 3)]
      (.insertNodeInto model node1 root 0)
      (.insertNodeInto model node2 root 1)
      (.insertNodeInto model node3 root 2)
      (should= 3 (.getChildCount root))
      (delete-last-event-if-too-many model 1)
      (should= 1 (.getChildCount root))))

  (it "does not delete last node if not too many"
    (let [root (DefaultMutableTreeNode. 0)
          model (DefaultTreeModel. root)
          node1 (DefaultMutableTreeNode. 1)
          node2 (DefaultMutableTreeNode. 2)
          node3 (DefaultMutableTreeNode. 3)]
      (.insertNodeInto model node1 root 0)
      (.insertNodeInto model node2 root 1)
      (.insertNodeInto model node3 root 2)
      (should= 3 (.getChildCount root))
      (delete-last-event-if-too-many model 3)
      (should= 3 (.getChildCount root))))

  (it "prunes tabs"
    (with-redefs [config/max-nodes-per-tab 2]
      (let [root (DefaultMutableTreeNode. 0)
            model (DefaultTreeModel. root)
            node1 (DefaultMutableTreeNode. 1)
            node2 (DefaultMutableTreeNode. 2)
            node3 (DefaultMutableTreeNode. 3)
            the-tree (tree :model model)]
        (set-mem :tabs-list [{:name "tab"}])
        (set-mem :tab-tree-map {"tab" the-tree})
        (.insertNodeInto model node1 root 0)
        (.insertNodeInto model node2 root 1)
        (.insertNodeInto model node3 root 2)
        (should= 3 (.getChildCount root))
        (prune-tabs)
        (should= 2 (.getChildCount root)))))

  )
