(ns more-speech.ui.swing.article-tree-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.swing.article-tree :refer :all]
            [more-speech.ui.swing.article-tree-util :refer :all]
            [more-speech.nostr.util :as util :refer [hexify]]
            [more-speech.mem :refer :all]
            [more-speech.db.gateway :as gateway]
            [more-speech.db.in-memory :as in-memory]
            [more-speech.config :as config])
  (:import (javax.swing.tree DefaultMutableTreeNode)))

(declare db)

(describe "header tree"
  (with db (in-memory/get-db))
  (before-all (config/set-db! :in-memory))
  (before (in-memory/clear-db @db)
          (clear-mem))

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

  (context "adding references to tree nodes"
    (it "adds no node reference if the event has no references"
      (let [id 1N
            _ (set-mem :node-map {id []})
            event {:id id :tags []}]
        (add-references event)
        (should= [] (get-mem [:node-map id])))
      )

    (it "adds a node reference, and a tree element if the event has a reference to an existing event"
      (let [parent-id 2N
            id 1N
            parent-node (DefaultMutableTreeNode. parent-id)
            node-map {parent-id [parent-node]
                      id []}
            _ (set-mem :node-map node-map)
            event {:id id :tags [[:e (util/num32->hex-string parent-id)]]}
            _ (add-references event)
            nodes (get-mem [:node-map id])]
        (should= 1 (count nodes))
        (should= id (.getUserObject (first nodes)))
        (should= 1 (.getChildCount parent-node))
        (should= id (.getUserObject ^DefaultMutableTreeNode (.getChildAt parent-node 0)))
        )
      )

    (it "adds a orphan if the event has a reference to an event that doesn't exist"
      (let [parent-id 2N
            id 1N
            node-map {id []}
            _ (set-mem :node-map node-map)
            event {:id id :tags [[:e (util/num32->hex-string parent-id)]]}
            _ (add-references event)
            nodes (get-mem [:node-map id])]
        (should= 0 (count nodes))
        (should= {parent-id #{1N}} (get-mem :orphaned-references))
        )
      )
    )

  (context "resolving orphaned references"
    (it "has nothing to do if the event is not an orphan"
      (let [event-id 1N
            node-map {}
            orphaned-references {}
            _ (set-mem :node-map node-map)
            _ (set-mem :orphaned-references orphaned-references)]
        (resolve-any-orphans event-id)
        (should= {} (get-mem :node-map))
        (should= {} (get-mem :orphaned-references)))
      )

    (it "resolves a parent event with a single orphan"
      (let [parent-id 1N
            orphan-id 2N
            parent-node (DefaultMutableTreeNode. parent-id)
            original-orphan-node (DefaultMutableTreeNode. orphan-id)
            node-map {orphan-id [original-orphan-node]
                      parent-id [parent-node]}
            orphaned-references {parent-id #{orphan-id}}
            _ (set-mem :node-map node-map)
            _ (set-mem :orphaned-references orphaned-references)
            _ (resolve-any-orphans parent-id)
            orphan-nodes (get-mem [:node-map orphan-id])
            new-orphan-node (second orphan-nodes)
            ]
        (should= 2 (count orphan-nodes))
        (should= 1 (.getChildCount parent-node))
        (should= orphan-id (-> parent-node
                               ^DefaultMutableTreeNode (.getChildAt 0)
                               .getUserObject))
        (should= orphan-id (.getUserObject new-orphan-node))
        (should= #{} (get-mem [:orphaned-references parent-id]))
        )
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

  (context "avoiding duplicate children in a node"
    (it "does not find a child in an empty node"
      (let [node (DefaultMutableTreeNode. nil)]
        (should-not (node-contains? node 1))))

    (it "finds first child"
      (let [node (DefaultMutableTreeNode. nil)
            child (DefaultMutableTreeNode. 1)
            _ (.add node child)]
        (should (node-contains? node 1))
        (should-not (node-contains? node 2))))

    (it "finds children from beginning to end"
      (let [node (DefaultMutableTreeNode. nil)
            child1 (DefaultMutableTreeNode. 1)
            child2 (DefaultMutableTreeNode. 2)
            child3 (DefaultMutableTreeNode. 3)
            _ (.add node child1)
            _ (.add node child2)
            _ (.add node child3)]
        (should (node-contains? node 1))
        (should (node-contains? node 2))
        (should (node-contains? node 3))
        (should-not (node-contains? node 4)))))
  )

