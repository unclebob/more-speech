(ns more-speech.ui.swing.article-tree
  (:require
    [more-speech.ui.swing.article-tree-util :refer :all]
    [more-speech.nostr.events :as events]
    [more-speech.ui.formatters :as formatters]
    [more-speech.ui.config :as config]
    [more-speech.ui.swing.article-panel :as article-panel])
  (:use [seesaw core font tree])
  (:import (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel TreePath)))

(declare render-event select-article)

(defn make-article-tree [event-agent main-frame]
  (let [header-tree (tree :renderer (partial render-event event-agent)
                          :root-visible? false
                          :expands-selected-paths? true
                          :model (DefaultTreeModel. (DefaultMutableTreeNode. "Empty"))
                          :id :header-tree)]
    (listen header-tree :selection (partial select-article event-agent main-frame))
    header-tree))

(defn select-article [event-agent main-frame e]
  (when (and
          (some? (last (selection e)))
          (instance? DefaultMutableTreeNode (last (selection e))))
    (let [selected-node (last (selection e))
          selected-id (.getUserObject selected-node)
          event-state @event-agent]
      (send event-agent events/add-read-event selected-id)
      (article-panel/load-article-info event-state selected-id main-frame))))

(defn render-event [event-agent widget info]
  (if (seqable? (:value info))
    (text! widget "Articles")
    (let [event-state @event-agent
          nicknames (:nicknames event-state)
          event-map (:text-event-map event-state)
          node (:value info)
          event-id (.getUserObject node)
          event (get event-map event-id)
          read? (contains? (:read-event-ids @event-agent) event-id)
          font (if read? config/default-font config/bold-font)]
      (config! widget :font font)
      (text! widget (formatters/format-header nicknames event)))))

(declare add-references resolve-any-orphans)

(defn add-event [ui-context event]
  (let [frame (:frame @ui-context)
        event-state @(:event-agent @ui-context)
        event-map (:text-event-map event-state)
        event-id (:id event)
        tree (select frame [:#header-tree])
        model (config tree :model)
        root (.getRoot model)
        insertion-point (find-chronological-insertion-point root event-id event-map)
        child (DefaultMutableTreeNode. event-id)]
    (.insertNodeInto model child root insertion-point)
    (.makeVisible tree (TreePath. (.getPath child)))
    (swap! ui-context update-in [:node-map event-id] conj child)
    (resolve-any-orphans ui-context event-id)
    (add-references ui-context event)
    ))

(defn resolve-any-orphans [ui-context parent-id]
  (let [parent-node (first (get-in @ui-context [:node-map parent-id]))
        orphans (get-in @ui-context [:orphaned-references parent-id])]
    (if (empty? orphans)
      nil
      (do
        (loop [orphans orphans]
          (if (empty? orphans)
            nil
            (let [orphan-id (first orphans)
                  orphan-node (DefaultMutableTreeNode. orphan-id)]
              (.add ^DefaultMutableTreeNode parent-node orphan-node)
              (swap! ui-context update-in [:node-map orphan-id] conj orphan-node)
              (recur (rest orphans)))))
        (swap! ui-context assoc-in [:orphaned-references parent-id] nil))))
  )

;; at the moment an event can appear in several places in the tree.
;; it can be in the reply chain of an event, and it can stand alone.
;; The node-map holds the list of nodes that correspond to the id of
;; an event.

(declare add-orphaned-reference add-this-node-to-reference-nodes)

(defn add-references [ui-context event]
  (let [[_ _ referent] (events/get-references event)
        id (:id event)]
    (if (nil? referent)
      nil
      (let [nodes (get-in @ui-context [:node-map referent])]
        (if (empty? nodes)
          (add-orphaned-reference ui-context referent id)
          (add-this-node-to-reference-nodes ui-context nodes id)
          )))))


(defn add-orphaned-reference [ui-context referent id]
  (swap! ui-context update-in [:orphaned-references referent] conj id))

(defn add-this-node-to-reference-nodes [ui-context reference-nodes this-id]
  (loop [nodes reference-nodes]
    (if (empty? nodes)
      nil
      (let [node (first nodes)
            child (DefaultMutableTreeNode. this-id)]
        (.add ^DefaultMutableTreeNode node child)
        (swap! ui-context update-in [:node-map this-id] conj child)
        (recur (rest nodes))))))