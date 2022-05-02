(ns more-speech.ui.swing.main-window-spec
  (:require [speclj.core :refer :all]
           [more-speech.ui.swing.main-window :refer :all]
           )
  (:import (javax.swing.tree DefaultMutableTreeNode)))

(describe "header tree"
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