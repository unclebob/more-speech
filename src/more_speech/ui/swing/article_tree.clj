(ns more-speech.ui.swing.article-tree
  (:require
    [more-speech.ui.swing.article-tree-util :refer :all]
    [more-speech.nostr.events :as events]
    [more-speech.ui.formatters :as formatters]
    [more-speech.ui.config :as config]
    [more-speech.ui.swing.article-panel :as article-panel]
    [more-speech.ui.swing.ui-context :refer :all])
  (:use [seesaw core font tree])
  (:import (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel TreePath)))

(declare render-event node-selected)

(defn make-header-tree [tab-name]
  (let [header-tree (tree :renderer render-event
                          :root-visible? false
                          :expands-selected-paths? true
                          :model (DefaultTreeModel. (DefaultMutableTreeNode. "Empty")))]
    (listen header-tree :selection (partial node-selected tab-name))
    header-tree))

(defn select-article [tab-name selected-node]
  (let [selected-id (.getUserObject selected-node)
        event-agent (:event-agent @ui-context)]
    (swap! ui-context assoc :selected-tab tab-name)
    (send event-agent events/select-event selected-id)
    (article-panel/load-article-info selected-id)))

(defn node-selected [tab-name e]
  (let [selected-node (last (selection e))]
    (when (and
            (some? selected-node)
            (instance? DefaultMutableTreeNode selected-node))
      (select-article tab-name selected-node))))

(defn render-event [widget info]
  (if (seqable? (:value info))
    (text! widget "Articles")
    (let [event-agent (:event-agent @ui-context)
          event-state @event-agent
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

(defn should-add-event? [filter event]
  (let [selected (:selected filter)
        blocked (:blocked filter)
        [root _mentions _referent] (events/get-references event)]
    (and
      (or
        (empty? selected)
        (some #(= % (:pubkey event)) selected)
        (some #(= % (:id event)) selected)
        (some #(= % root) selected))
      (not
        (or
          (some #(= % (:pubkey event)) blocked)
          (some #(= % (:id event)) blocked))))
    )
  )

(defn add-event [event]
  (let [frame (:frame @ui-context)
        event-state @(:event-agent @ui-context)
        event-map (:text-event-map event-state)
        event-id (:id event)
        tabs (:tabs event-state)]
    (loop [tab-names (keys tabs)]
      (if (empty? tab-names)
        nil
        (let [tree-id (keyword (str "#" (name (first tab-names))))
              tree (select frame [tree-id])
              filter (config tree :user-data)]
          (when (should-add-event? filter event)
            (let [model (config tree :model)
                  root (.getRoot model)
                  insertion-point (find-chronological-insertion-point root event-id event-map)
                  child (DefaultMutableTreeNode. event-id)]
              (.insertNodeInto model child root insertion-point)
              (.makeVisible tree (TreePath. (.getPath child)))
              (swap! ui-context update-in [:node-map event-id] conj child)
              (resolve-any-orphans ui-context event-id)
              (add-references ui-context event)
              ))
          (recur (rest tab-names)))))))

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
          (add-orphaned-reference referent id)
          (add-this-node-to-reference-nodes nodes id)
          )))))


(defn add-orphaned-reference [referent id]
  (swap! ui-context update-in [:orphaned-references referent] conj id))

(defn add-this-node-to-reference-nodes [reference-nodes this-id]
  (loop [nodes reference-nodes]
    (if (empty? nodes)
      nil
      (let [node (first nodes)
            child (DefaultMutableTreeNode. this-id)]
        (.add ^DefaultMutableTreeNode node child)
        (swap! ui-context update-in [:node-map this-id] conj child)
        (recur (rest nodes))))))