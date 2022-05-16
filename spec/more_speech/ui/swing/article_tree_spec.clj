(ns more-speech.ui.swing.article-tree-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.swing.article-tree :refer :all]
            )
  (:import (javax.swing.tree DefaultMutableTreeNode)))

(describe "header tree"
  (context "finding chronological insertion point"
    (it "returns zero if empty tree"
      (let [root (DefaultMutableTreeNode.)
            time 1
            event-map {}
            insertion-point (find-chronological-insertion-point root time event-map)]
        (should= 0 insertion-point)))

    (it "returns zero if time is earlier than all events in tree"
      (let [root (DefaultMutableTreeNode.)
            child-id 1
            child (DefaultMutableTreeNode. child-id)
            _ (.add ^DefaultMutableTreeNode root child)
            event-map {child-id {:created-at 10}}
            event-time 1                                    ;earlier than 10
            insertion-point (find-chronological-insertion-point root event-time event-map)]
        (should= 0 insertion-point)))

    (it "returns 1 when event is later than only event in tree"
      (let [root (DefaultMutableTreeNode.)
            child-id 1
            child (DefaultMutableTreeNode. child-id)
            _ (.add ^DefaultMutableTreeNode root child)
            event-map {child-id {:created-at 10}}
            event-time 20                                   ;later than 10
            insertion-point (find-chronological-insertion-point root event-time event-map)]
        (should= 1 insertion-point))
      )

    (it "returns n when event is later than n events in tree"
      (let [root (DefaultMutableTreeNode.)
            child-id 1
            child-1 (DefaultMutableTreeNode. child-id)
            child-2 (DefaultMutableTreeNode. (+ 1 child-id))
            child-3 (DefaultMutableTreeNode. (+ 2 child-id))
            _ (.add ^DefaultMutableTreeNode root child-1)
            _ (.add ^DefaultMutableTreeNode root child-2)
            _ (.add ^DefaultMutableTreeNode root child-3)
            event-map {child-id {:created-at 10}
                       (+ 1 child-id) {:created-at 10}
                       (+ 2 child-id) {:created-at 10}}
            event-time 20                                   ;later than 10
            insertion-point (find-chronological-insertion-point root event-time event-map)]
        (should= 3 insertion-point)))

    (it "returns chronological insertion point"
          (let [root (DefaultMutableTreeNode.)
                child-id 1
                child-1 (DefaultMutableTreeNode. child-id)
                child-2 (DefaultMutableTreeNode. (+ 1 child-id))
                child-3 (DefaultMutableTreeNode. (+ 2 child-id))
                _ (.add ^DefaultMutableTreeNode root child-1)
                _ (.add ^DefaultMutableTreeNode root child-2)
                _ (.add ^DefaultMutableTreeNode root child-3)
                event-map {child-id {:created-at 10}
                           (+ 1 child-id) {:created-at 20}
                           (+ 2 child-id) {:created-at 30}}
                event-time 25
                insertion-point (find-chronological-insertion-point root event-time event-map)]
            (should= 2 insertion-point))
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

    ))