(ns more-speech.ui.swing.article-tree-spec
  (:use [seesaw core font tree])
  (:require [speclj.core :refer :all]
            [more-speech.ui.swing.article-tree :refer :all]
            [more-speech.ui.swing.article-tree-util :refer :all]
            [more-speech.nostr.util :as util]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.ui.swing.article-panel :as article-panel])
  (:import (javax.swing.tree DefaultMutableTreeNode)))

(defn hexify [n] (util/num32->hex-string n))

(describe "header tree"
  (context "finding chronological insertion point"
    (it "returns zero if empty tree"
      (let [root (DefaultMutableTreeNode.)
            event {:id 99 :created-at 1}
            event-map {99 event}
            insertion-point (find-chronological-insertion-point root 99 event-map)]
        (should= 0 insertion-point)))

    (it "returns zero if time is later than all events in tree"
      (let [root (DefaultMutableTreeNode.)
            child-id 1
            child (DefaultMutableTreeNode. child-id)
            _ (.add ^DefaultMutableTreeNode root child)
            event {:id 99 :created-at 20}
            event-map {99 event
                       child-id {:created-at 10}}
            insertion-point (find-chronological-insertion-point root 99 event-map)]
        (should= 0 insertion-point)))

    (it "returns 1 when event is ealier than only event in tree"
      (let [root (DefaultMutableTreeNode.)
            child-id 1
            child (DefaultMutableTreeNode. child-id)
            _ (.add ^DefaultMutableTreeNode root child)
            event {:id 99 :created-at 5}
            event-map {99 event
                       child-id {:created-at 10}}
            insertion-point (find-chronological-insertion-point root 99 event-map)]
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
            event {:id 99 :created-at 5}
            event-map {99 event
                       child-id {:created-at 10}
                       (+ 1 child-id) {:created-at 10}
                       (+ 2 child-id) {:created-at 10}}
            insertion-point (find-chronological-insertion-point root 99 event-map)]
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
            event {:id 99 :created-at 15}
            event-map {99 event
                       child-id {:created-at 30}
                       (+ 1 child-id) {:created-at 20}
                       (+ 2 child-id) {:created-at 10}}
            insertion-point (find-chronological-insertion-point root 99 event-map)]
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
            event {:id 99 :created-at 20}
            event-map {99 event
                       child-id {:created-at 10}
                       (+ 1 child-id) {:created-at 20}
                       (+ 2 child-id) {:created-at 30}}
            insertion-point (find-chronological-insertion-point root 99 event-map)]
        (should= 1 insertion-point))
      )
    )

  (context "adding references to tree nodes"
    (it "adds no node reference if the event has no references"
      (let [id 1N
            node-map {id []}
            ui-context (atom {:node-map node-map})
            event {:id id :tags []}]
        (add-references event)
        (should= [] (get-in @ui-context [:node-map id])))
      )

    (it "adds a node reference, and a tree element if the event has a reference to an existing event"
      (let [parent-id 2N
            id 1N
            parent-node (DefaultMutableTreeNode. parent-id)
            node-map {parent-id [parent-node]
                      id []}
            _ (reset! ui-context {:node-map node-map})
            event {:id id :tags [[:e (util/num32->hex-string parent-id)]]}
            _ (add-references event)
            nodes (get-in @ui-context [:node-map id])]
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
            _ (reset! ui-context {:node-map node-map})
            event {:id id :tags [[:e (util/num32->hex-string parent-id)]]}
            _ (add-references event)
            nodes (get-in @ui-context [:node-map id])]
        (should= 0 (count nodes))
        (should= {parent-id #{1N}} (:orphaned-references @ui-context))
        )
      )
    )

  (context "resolving orphaned references"
    (it "has nothing to do if the event is not an orphan"
      (let [event-id 1N
            node-map {}
            orphaned-references {}
            _ (reset! ui-context {:node-map node-map
                                  :orphaned-references orphaned-references})]
        (resolve-any-orphans event-id)
        (should= {} (:node-map @ui-context))
        (should= {} (:orphaned-references @ui-context)))
      )

    (it "resolves a parent event with a single orphan"
      (let [parent-id 1N
            orphan-id 2N
            parent-node (DefaultMutableTreeNode. parent-id)
            original-orphan-node (DefaultMutableTreeNode. orphan-id)
            node-map {orphan-id [original-orphan-node]
                      parent-id [parent-node]}
            orphaned-references {parent-id #{orphan-id}}
            _ (reset! ui-context {:node-map node-map
                                  :orphaned-references orphaned-references})
            _ (resolve-any-orphans parent-id)
            orphan-nodes (get-in @ui-context [:node-map orphan-id])
            new-orphan-node (second orphan-nodes)
            ]
        (should= 2 (count orphan-nodes))
        (should= 1 (.getChildCount parent-node))
        (should= orphan-id (-> parent-node
                               ^DefaultMutableTreeNode (.getChildAt 0)
                               .getUserObject))
        (should= orphan-id (.getUserObject new-orphan-node))
        (should= #{} (get-in @ui-context [:orphaned-references parent-id]))
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
    (it "remembers which articles have been read and loads article"
      (with-redefs
        [article-panel/load-article-info
         (stub :load-article-info {:return nil})]
        (let [event-context (atom {:read-event-ids #{}
                                   :selected-event nil
                                   :event-history []
                                   :back-count 1})
              _ (reset! ui-context {:event-context event-context})
              selected-event-id 1
              selected-node (DefaultMutableTreeNode. selected-event-id)
              tab-name :bob-tab
              _ (select-article tab-name selected-node)
              event-state @event-context
              read-event-ids (:read-event-ids event-state)
              selected-event (:selected-event event-state)
              event-history (:event-history event-state)
              back-count (:back-count event-state)]
          (should-have-invoked :load-article-info {:with [selected-event-id]})
          (should= read-event-ids #{selected-event-id})
          (should= selected-event selected-event-id)
          (should= [[tab-name selected-event-id]] event-history)
          (should= 0 back-count)))))
  )

(declare depict-tree
         make-event-map
         add-events)

(describe "adding events"
  (with-stubs)

  (it "adds one event"
    (let [tab-id :tab
          filter {:selected []
                  :blocked []}
          header-tree (make-header-tree tab-id)
          _ (config! header-tree :id tab-id :user-data filter)
          frame (frame :content header-tree)
          event-id 99
          event {:id event-id}
          event-state {:tabs {tab-id nil}}
          event-context (atom event-state)]
      (reset! ui-context {:frame frame
                          :event-context event-context})
      (add-event event)
      (should= ["Root" [event-id]] (depict-tree header-tree))))

  (it "adds two events"
    (with-redefs [render-event (stub :render-event)]
      (let [tab-id :tab
            filter {:selected []
                    :blocked []}
            header-tree (make-header-tree tab-id)
            _ (config! header-tree :id tab-id :user-data filter)
            frame (frame :content header-tree)
            event-list [{:id 99 :created-at 2} {:id 88 :created-at 1}]
            event-map (make-event-map event-list)
            event-state {:tabs {tab-id nil}
                         :text-event-map event-map}
            event-context (atom event-state)]
        (reset! ui-context {:frame frame
                            :event-context event-context})
        (add-events event-list)
        (should= ["Root" [99] [88]] (depict-tree header-tree)))))

  (it "adds four events and keeps them in reverse chronologial order"
    (with-redefs [render-event (stub :render-event)]
      (let [tab-id :tab
            filter {:selected []
                    :blocked []}
            header-tree (make-header-tree tab-id)
            _ (config! header-tree :id tab-id :user-data filter)
            frame (frame :content header-tree)
            event-list [{:id 99 :created-at 1}
                        {:id 88 :created-at 2}
                        {:id 77 :created-at 3}
                        {:id 66 :created-at 4}]
            event-map (make-event-map event-list)
            event-state {:tabs {tab-id nil}
                         :text-event-map event-map}
            event-context (atom event-state)]
        (reset! ui-context {:frame frame
                            :event-context event-context})
        (add-events event-list)
        (should= ["Root" [66] [77] [88] [99]] (depict-tree header-tree)))))

  (it "adds an event, and a reply when received in order."
    (with-redefs [render-event (stub :render-event)]
      (let [tab-id :tab
            filter {:selected []
                    :blocked []}
            header-tree (make-header-tree tab-id)
            _ (config! header-tree :id tab-id :user-data filter)
            frame (frame :content header-tree)
            event-list [{:id 99 :created-at 1}
                        {:id 88 :created-at 2 :tags [[:e (hexify 99) "" "reply"]]}
                        ]
            event-map (make-event-map event-list)
            event-state {:tabs {tab-id nil}
                         :text-event-map event-map}
            event-context (atom event-state)]
        (reset! ui-context {:frame frame
                            :event-context event-context})
        (add-events event-list)
        (should= ["Root" [88] [99 [88]]] (depict-tree header-tree)))))

  (it "adds an event, and a reply when received out of order."
      (with-redefs [render-event (stub :render-event)]
        (let [tab-id :tab
              filter {:selected []
                      :blocked []}
              header-tree (make-header-tree tab-id)
              _ (config! header-tree :id tab-id :user-data filter)
              frame (frame :content header-tree)
              event-list [{:id 88 :created-at 2 :tags [[:e (hexify 99) "" "reply"]]}
                          {:id 99 :created-at 1}]
              event-map (make-event-map event-list)
              event-state {:tabs {tab-id nil}
                           :text-event-map event-map}
              event-context (atom event-state)]
          (reset! ui-context {:frame frame
                              :event-context event-context})
          (add-events event-list)
          (should= ["Root" [88] [99 [88]]] (depict-tree header-tree)))))

  (it "adds a chain of replies in order."
      (with-redefs [render-event (stub :render-event)]
        (let [tab-id :tab
              filter {:selected []
                      :blocked []}
              header-tree (make-header-tree tab-id)
              _ (config! header-tree :id tab-id :user-data filter)
              frame (frame :content header-tree)
              event-list [{:id 99 :created-at 1}
                          {:id 88 :created-at 2 :tags [[:e (hexify 99) "" "reply"]]}
                          {:id 77 :created-at 3 :tags [[:e (hexify 88) "" "reply"]]}
                          {:id 66 :created-at 4 :tags [[:e (hexify 77) "" "reply"]]}
                          {:id 55 :created-at 5 :tags [[:e (hexify 88) "" "reply"]]}
                          ]
              event-map (make-event-map event-list)
              event-state {:tabs {tab-id nil}
                           :text-event-map event-map}
              event-context (atom event-state)]
          (reset! ui-context {:frame frame
                              :event-context event-context})
          (add-events event-list)
          (should= ["Root"  [55] [66] [77 [66]] [88 [77 [66]] [55]] [99 [88 [77 [66]] [55]]]] (depict-tree header-tree)))))

  (it "adds a chain of replies in reverse order."
        (with-redefs [render-event (stub :render-event)]
          (let [tab-id :tab
                filter {:selected []
                        :blocked []}
                header-tree (make-header-tree tab-id)
                _ (config! header-tree :id tab-id :user-data filter)
                frame (frame :content header-tree)
                event-list [{:id 99 :created-at 1}
                            {:id 88 :created-at 2 :tags [[:e (hexify 99) "" "reply"]]}
                            {:id 77 :created-at 3 :tags [[:e (hexify 88) "" "reply"]]}
                            {:id 66 :created-at 4 :tags [[:e (hexify 77) "" "reply"]]}
                            {:id 55 :created-at 5 :tags [[:e (hexify 88) "" "reply"]]}
                            ]
                event-list (reverse event-list)
                event-map (make-event-map event-list)
                event-state {:tabs {tab-id nil}
                             :text-event-map event-map}
                event-context (atom event-state)]
            (reset! ui-context {:frame frame
                                :event-context event-context})
            (add-events event-list)
            (should= ["Root"  [55] [66] [77 [66]] [88 [77 [66]] [55]] [99 [88 [77 [66]] [55]]]] (depict-tree header-tree)))))
  )

(declare depict-node)

(defn depict-tree [tree]
  (let [model (config tree :model)
        root (.getRoot model)]
    (depict-node model root)))

(defn depict-node [model node]
  (loop [ns (range (.getChildCount node))
         node-depiction [(.getUserObject node)]]
    (if (empty? ns)
      node-depiction
      (let [n (first ns)
            child (.getChild model node n)]
        (recur (rest ns) (conj node-depiction (depict-node model child)))))))

(defn make-event-map [event-list]
  (loop [event-list event-list
         event-map {}]
    (if (empty? event-list)
      event-map
      (let [event (first event-list)]
        (recur (rest event-list) (assoc event-map (:id event) event))))))

(defn add-events [event-list]
  (doseq [event event-list]
    (add-event event)))