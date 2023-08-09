(ns more-speech.ui.swing.tabs-util-spec
  (:require [more-speech.db.gateway :as gateway]
            [more-speech.mem :refer :all]
            [more-speech.spec-util :refer :all]
            [more-speech.ui.swing.tabs-util :refer :all]
            [speclj.core :refer :all])
(:import (javax.swing.tree DefaultMutableTreeNode)) )

(declare db)

(describe "header tree"
  (setup-db-mem)

  (context "finding chronological insertion point"
    (it "returns zero if empty tree"
      (let [root (DefaultMutableTreeNode.)
            _ (gateway/add-event @db {:id 99 :created-at 1})
            insertion-point (find-chronological-insertion-point root 99)]
        (should= 0 insertion-point)))

    (it "returns zero if time is later than all events in tree"
      (let [root (DefaultMutableTreeNode.)
            child-id 1
            child (DefaultMutableTreeNode. child-id)
            _ (.add ^DefaultMutableTreeNode root child)
            _ (gateway/add-event @db {:id 99 :created-at 20})
            _ (gateway/add-event @db {:id child-id :created-at 10})
            insertion-point (find-chronological-insertion-point root 99)]
        (should= 0 insertion-point)))

    (it "returns 1 when event is ealier than only event in tree"
      (let [root (DefaultMutableTreeNode.)
            child-id 1
            child (DefaultMutableTreeNode. child-id)
            _ (.add ^DefaultMutableTreeNode root child)
            _ (gateway/add-event @db {:id 99 :created-at 5})
            _ (gateway/add-event @db {:id child-id :created-at 10})
            insertion-point (find-chronological-insertion-point root 99)]
        (should= 1 insertion-point))
      )

    (it "returns n when event is earlier than n events in tree"
      (let [root (DefaultMutableTreeNode.)
            child-id 1
            child-1 (DefaultMutableTreeNode. child-id)
            child-2 (DefaultMutableTreeNode. (+ 1 child-id))
            child-3 (DefaultMutableTreeNode. (+ 2 child-id))
            _ (.add ^DefaultMutableTreeNode root child-1)
            _ (.add ^DefaultMutableTreeNode root child-2)
            _ (.add ^DefaultMutableTreeNode root child-3)
            event-map {99 {:id 99 :created-at 5}
                       child-id {:id child-id :created-at 10}
                       (+ 1 child-id) {:id (+ 1 child-id) :created-at 10}
                       (+ 2 child-id) {:id (+ 2 child-id) :created-at 10}}
            _ (doseq [id (keys event-map)]
                (gateway/add-event @db (get event-map id)))
            insertion-point (find-chronological-insertion-point root 99)]
        (should= 3 insertion-point)))

    (it "returns chronological insertion point above first earliest"
      (let [root (DefaultMutableTreeNode.)
            child-id 1
            child-1 (DefaultMutableTreeNode. child-id)
            child-2 (DefaultMutableTreeNode. (+ 1 child-id))
            child-3 (DefaultMutableTreeNode. (+ 2 child-id))
            _ (.add ^DefaultMutableTreeNode root child-1)
            _ (.add ^DefaultMutableTreeNode root child-2)
            _ (.add ^DefaultMutableTreeNode root child-3)
            event-map {99 {:id 99 :created-at 15}
                       child-id {:id child-id :created-at 30}
                       (+ 1 child-id) {:id (+ 1 child-id) :created-at 20}
                       (+ 2 child-id) {:id (+ 2 child-id) :created-at 10}}
            _ (doseq [id (keys event-map)]
                (gateway/add-event @db (get event-map id)))
            insertion-point (find-chronological-insertion-point root 99)]
        (should= 2 insertion-point))
      )

    (it "returns chronological insertion point when coincident"
      (let [root (DefaultMutableTreeNode.)
            child-id 1
            child-1 (DefaultMutableTreeNode. child-id)
            child-2 (DefaultMutableTreeNode. (+ 1 child-id))
            child-3 (DefaultMutableTreeNode. (+ 2 child-id))
            _ (.add ^DefaultMutableTreeNode root child-1)
            _ (.add ^DefaultMutableTreeNode root child-2)
            _ (.add ^DefaultMutableTreeNode root child-3)
            event-map {99 {:id 99 :created-at 20}
                       child-id {:id child-id :created-at 10}
                       (+ 1 child-id) {:id (+ 1 child-id) :created-at 20}
                       (+ 2 child-id) {:id (+ 2 child-id) :created-at 30}}
            _ (doseq [id (keys event-map)]
                (gateway/add-event @db (get event-map id)))
            insertion-point (find-chronological-insertion-point root 99)]
        (should= 1 insertion-point))
      )
    )

  (context "finding nodes"
    (it "finds nothing in an empty tree"
      (let [root (DefaultMutableTreeNode.)
            found-node (find-header-node root 1)]
        (should-be-nil found-node)))

    (it "finds nothing in non-empty tree"
      (let [root (DefaultMutableTreeNode.)
            child (DefaultMutableTreeNode. 2)
            _ (.add ^DefaultMutableTreeNode root child)
            found-node (find-header-node root 1)]
        (should-be-nil found-node)))

    (it "finds node in one-node tree"
      (let [root (DefaultMutableTreeNode.)
            child (DefaultMutableTreeNode. 1)
            _ (.add ^DefaultMutableTreeNode root child)
            found-node (find-header-node root 1)]
        (should-not-be-nil found-node)
        (should= 1 (.getUserObject found-node))))

    (it "finds node in multi-node non-branching tree"
      (let [root (DefaultMutableTreeNode.)
            child1 (DefaultMutableTreeNode. 1)
            child2 (DefaultMutableTreeNode. 2)
            child3 (DefaultMutableTreeNode. 3)
            _ (.add ^DefaultMutableTreeNode root child1)
            _ (.add ^DefaultMutableTreeNode root child2)
            _ (.add ^DefaultMutableTreeNode root child3)
            found-node (find-header-node root 2)]
        (should-not-be-nil found-node)
        (should= 2 (.getUserObject found-node))))

    (it "does not find node in multi-node non-branching tree"
      (let [root (DefaultMutableTreeNode.)
            child1 (DefaultMutableTreeNode. 1)
            child2 (DefaultMutableTreeNode. 2)
            child3 (DefaultMutableTreeNode. 3)
            _ (.add ^DefaultMutableTreeNode root child1)
            _ (.add ^DefaultMutableTreeNode root child2)
            _ (.add ^DefaultMutableTreeNode root child3)
            found-node (find-header-node root 4)]
        (should-be-nil found-node)))

    (it "finds node in multi-node branching tree"
      (let [root (DefaultMutableTreeNode.)
            child1 (DefaultMutableTreeNode. 1)
            child2 (DefaultMutableTreeNode. 2)
            child3 (DefaultMutableTreeNode. 3)
            _ (.add ^DefaultMutableTreeNode root child1)
            _ (.add ^DefaultMutableTreeNode child1 child2)
            _ (.add ^DefaultMutableTreeNode root child3)
            found-node (find-header-node root 2)]
        (should-not-be-nil found-node)
        (should= 2 (.getUserObject found-node))))

    (it "Does not find node in multi-node branching tree"
      (let [root (DefaultMutableTreeNode.)
            child1 (DefaultMutableTreeNode. 1)
            child2 (DefaultMutableTreeNode. 2)
            child3 (DefaultMutableTreeNode. 3)
            _ (.add ^DefaultMutableTreeNode root child1)
            _ (.add ^DefaultMutableTreeNode child1 child2)
            _ (.add ^DefaultMutableTreeNode root child3)
            found-node (find-header-node root 4)]
        (should-be-nil found-node)))
    )

  (context "tab names"
    (it "gets list of changeable tab names"
              (set-mem :tabs-list [{:name "all" :selected [1] :blocked [2]}
                                   {:name "tab" :selected [3] :blocked [4]}
                                   {:name "trusted" :selected [:trusted] :blocked []}])
              (should= ["tab"] (get-changeable-tab-names)))

    (it "gets list of changeable tab descriptors"
              (set-mem :tabs-list [{:name "all" :selected [1] :blocked [2]}
                                   {:name "tab" :selected [3] :blocked [4]}
                                   {:name "trusted" :selected [:trusted] :blocked []}])
              (should= [{:name "tab" :selected [3] :blocked [4]}] (get-changeable-tab-descriptors))))
  )

