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
      (article-panel/load-article-info event-state selected-id main-frame))))

(defn render-event [event-agent widget info]
  (config! widget :font config/default-font)
  (if (seqable? (:value info))
    (text! widget "Articles")
    (let [event-state @event-agent
          nicknames (:nicknames event-state)
          event-map (:text-event-map event-state)
          node (:value info)
          event-id (.getUserObject node)
          event (get event-map event-id)]
      (text! widget (formatters/format-header nicknames event)))))

(declare add-references)

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
    (add-references ui-context event)
    ))

;; at the moment an event can appear in several places in the tree.
;; it can be in the reply chain of an event, and it can stand alone.
;; The node-map holds the list of nodes that correspond to the id of
;; an event.

(defn add-references [ui-context event]
  (let [[_ _ referent] (events/get-references event)
        id (:id event)]
    (if (nil? referent)
      nil
      (loop [nodes (get-in @ui-context [:node-map referent])]
        (if (empty? nodes)
          nil
          (let [node (first nodes)
                child (DefaultMutableTreeNode. id)]
            (.add ^DefaultMutableTreeNode node child)
            (swap! ui-context update-in [:node-map id] conj child)
            (recur (rest nodes))))))))
