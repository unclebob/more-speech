(ns more-speech.ui.swing.article-tree-spec
  (:use [seesaw core font tree])
  (:require [speclj.core :refer :all]
            [more-speech.ui.swing.article-tree :refer :all]
            [more-speech.ui.swing.article-tree-util :refer :all]
            [more-speech.nostr.util :as util]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.ui.swing.article-panel :as article-panel]
            [more-speech.ui.swing.util :as swing-util]
            [more-speech.db.gateway :as gateway]
            [more-speech.db.in-memory :as in-memory])
  (:import (javax.swing.tree DefaultMutableTreeNode)))

(defn hexify [n] (util/num32->hex-string n))

(declare db)

(describe "header tree"
  (with db (in-memory/get-db))
  (before (in-memory/clear-db @db)
          (clear-mem))

  (context "finding chronological insertion point"
    (it "returns zero if empty tree"
      (let [root (DefaultMutableTreeNode.)
            _ (gateway/add-event @db 99 {:id 99 :created-at 1})
            insertion-point (find-chronological-insertion-point root 99)]
        (should= 0 insertion-point)))

    (it "returns zero if time is later than all events in tree"
      (let [root (DefaultMutableTreeNode.)
            child-id 1
            child (DefaultMutableTreeNode. child-id)
            _ (.add ^DefaultMutableTreeNode root child)
            _ (gateway/add-event @db 99 {:id 99 :created-at 20})
            _ (gateway/add-event @db child-id {:created-at 10})
            insertion-point (find-chronological-insertion-point root 99)]
        (should= 0 insertion-point)))

    (it "returns 1 when event is ealier than only event in tree"
      (let [root (DefaultMutableTreeNode.)
            child-id 1
            child (DefaultMutableTreeNode. child-id)
            _ (.add ^DefaultMutableTreeNode root child)
            _ (gateway/add-event @db 99 {:id 99 :created-at 5})
            _ (gateway/add-event @db child-id {:created-at 10})
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
                       child-id {:created-at 10}
                       (+ 1 child-id) {:created-at 10}
                       (+ 2 child-id) {:created-at 10}}
            _ (doseq [id (keys event-map)]
                (gateway/add-event @db id (get event-map id)))
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
                       child-id {:created-at 30}
                       (+ 1 child-id) {:created-at 20}
                       (+ 2 child-id) {:created-at 10}}
            _ (doseq [id (keys event-map)]
                (gateway/add-event @db id (get event-map id)))
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
                       child-id {:created-at 10}
                       (+ 1 child-id) {:created-at 20}
                       (+ 2 child-id) {:created-at 30}}
            _ (doseq [id (keys event-map)]
                (gateway/add-event @db id (get event-map id)))
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
                    {:id 2 :pubkey 20 :tags [[:p (hexify 50)]]}
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

  (context "selecting nodes"
    (with-stubs)
    (with db (in-memory/get-db))
    (before (in-memory/clear-db @db))

    (it "remembers which articles have been read and loads article"
      (with-redefs
        [article-panel/load-article-info
         (stub :load-article-info {:return nil})]

        (let [selected-event-id 1
              _ (set-mem :back-count 1)
              _ (gateway/add-event @db selected-event-id {:content "event"})
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
          (should= 0 back-count))))))

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
    (gateway/add-event db (:id event) event)
    (add-event event)))

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

(declare db)
(describe "adding events"
  (with-stubs)
  (with db (in-memory/get-db))
  (before (in-memory/clear-db @db)
          (clear-mem))

  (it "adds one event"
    (should= ["Root" [99]]
             (events->tree @db
                           [{:id 99}])))

  (it "adds two events"
    (should= ["Root" [99] [88]]
             (events->tree @db
                           [{:id 99 :created-at 2}
                            {:id 88 :created-at 1}])))

  (it "adds four events and keeps them in reverse chronological order"
    (should= ["Root" [66] [77] [88] [99]]
             (events->tree @db
                           [{:id 99 :created-at 1}
                            {:id 88 :created-at 2}
                            {:id 77 :created-at 3}
                            {:id 66 :created-at 4}])))

  (it "adds an event, and a reply when received in order."
    (should= ["Root" [88] [99 [88]]]
             (events->tree @db
                           [{:id 99 :created-at 1}
                            {:id 88 :created-at 2 :tags [[:e (hexify 99) "" "reply"]]}])))

  (it "adds an event, and a reply when received out of order."
    (should= ["Root" [88] [99 [88]]]
             (events->tree @db
                           [{:id 88 :created-at 2 :tags [[:e (hexify 99) "" "reply"]]}
                            {:id 99 :created-at 1}])))

  (it "adds a complex chain of replies in order."
    (should= ["Root" [55] [66] [77 [66]] [88 [77 [66]] [55]] [99 [88 [77 [66]] [55]]]]
             (events->tree @db
                           [{:id 99 :created-at 1}
                            {:id 88 :created-at 2 :tags [[:e (hexify 99) "" "reply"]]}
                            {:id 77 :created-at 3 :tags [[:e (hexify 88) "" "reply"]]}
                            {:id 66 :created-at 4 :tags [[:e (hexify 77) "" "reply"]]}
                            {:id 55 :created-at 5 :tags [[:e (hexify 88) "" "reply"]]}])))

  (it "adds a chain of three replies in order."
    (should= ["Root" [77] [88 [77]] [99 [88 [77]]]]
             (events->tree @db
                           [{:id 99 :created-at 1}
                            {:id 88 :created-at 2 :tags [[:e (hexify 99) "" "reply"]]}
                            {:id 77 :created-at 3 :tags [[:e (hexify 88) "" "reply"]]}])))

  (it "adds a chain of three replies in reverse order."
    (should= ["Root" [77] [88 [77]] [99 [88 [77]]]]
             (events->tree @db
                           (reverse
                             [{:id 99 :created-at 1}
                              {:id 88 :created-at 2 :tags [[:e (hexify 99) "" "reply"]]}
                              {:id 77 :created-at 3 :tags [[:e (hexify 88) "" "reply"]]}]))))

  (it "adds a complex chain of replies in reverse order."
    (should= ["Root" [55] [66] [77 [66]] [88 [55] [77 [66]]] [99 [88 [55] [77 [66]]]]]
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
          copied-node (copy-node node)]
      (should= [1] (depict-node copied-node))))

  (it "copies a node with one child"
    (let [node (DefaultMutableTreeNode. 1)
          child (DefaultMutableTreeNode. 2)
          _ (.add node child)
          copied-node (copy-node node)]
      (should= [1 [2]] (depict-node copied-node))))

  (it "copies a node with two children"
    (let [node (DefaultMutableTreeNode. 1)
          _ (.add node (DefaultMutableTreeNode. 2))
          _ (.add node (DefaultMutableTreeNode. 3))
          copied-node (copy-node node)]
      (should= [1 [2] [3]] (depict-node copied-node))))

  (it "copies a node with two children and grandchildren"
    (let [node (DefaultMutableTreeNode. 1)
          child1 (DefaultMutableTreeNode. 2)
          _ (.add node child1)
          child2 (DefaultMutableTreeNode. 3)
          _ (.add node child2)
          _ (.add child2 (DefaultMutableTreeNode. 4))
          copied-node (copy-node node)]
      (should= [1 [2] [3 [4]]] (depict-node copied-node))))
  )

(describe "adding ids to tabs"
  (with-stubs)
  (with db (in-memory/get-db))

  (before (in-memory/clear-db @db))

  (it "adds an an unrooted article id to a tab"
    (gateway/add-event @db 1 {:tags []})
    (with-redefs [swing-util/add-id-to-tab (stub :add-id-to-tab)
                  swing-util/relaunch (stub :relaunch)]
      (add-article-to-tab 1 "tab" nil)
      (should-have-invoked :relaunch)
      (should-have-invoked :add-id-to-tab {:with ["tab" :selected 1]})))

  (it "adds the root id of a thread to a tab"
    (let [root-id 100
          event {:tags [[:e (hexify root-id) "" "root"]]}]
      (gateway/add-event @db 1 event)
      (with-redefs [swing-util/add-id-to-tab (stub :add-id-to-tab)
                    swing-util/relaunch (stub :relaunch)]
        (add-article-to-tab 1 "tab" nil)
        (should-have-invoked :relaunch)
        (should-have-invoked :add-id-to-tab {:with ["tab" :selected root-id]}))))
  )