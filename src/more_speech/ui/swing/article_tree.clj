(ns more-speech.ui.swing.article-tree
  (:require [more-speech.nostr.events :as events]
            [more-speech.ui.formatters :as formatters]
            [more-speech.nostr.elliptic-signature :as ecc]
            [more-speech.ui.config :as config])
  (:use [seesaw core font tree])
  (:import (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel TreePath)))

(declare render-event select-article)

(defn make-article-tree [event-agent main-frame]
  (let [header-tree (tree :renderer (partial render-event event-agent)
                          :root-visible? false
                          :model (DefaultTreeModel. (DefaultMutableTreeNode. "Empty"))
                          :id :header-tree)]
    (listen header-tree :selection (partial select-article event-agent main-frame))
    header-tree))

(defn select-article [event-agent main-frame e]
  (when (last (selection e))
    (let [selected-id (.getUserObject (last (selection e)))
          event-state @event-agent
          nicknames (:nicknames event-state)
          format-user (partial formatters/format-user-id nicknames)
          text-map (:text-event-map event-state)
          event (get text-map selected-id)
          [_ _ referent] (events/get-references event)
          reply-to (select main-frame [:#reply-to])
          citing (select main-frame [:#citing])
          article-area (select main-frame [:#article-area])]
      (text! article-area (formatters/reformat-article (:content event) 80))
      (text! (select main-frame [:#author-id])
             (format-user (:pubkey event)))
      (text! (select main-frame [:#created-at])
             (formatters/format-time (:created-at event)))
      (if (some? referent)
        (let [replied-event (get text-map referent)]
          (text! reply-to (format-user (:pubkey replied-event)))
          (text! citing (formatters/abbreviate (ecc/num32->hex-string referent) 32)))
        (do (text! reply-to "")
            (text! citing "")))
      )))

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
        event-id (:id event)
        tree (select frame [:#header-tree])
        model (config tree :model)
        root (.getRoot model)
        child-count (.getChildCount root)
        child (DefaultMutableTreeNode. event-id)]
    (.insertNodeInto model child root child-count)
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

(defn find-header-node [root id]
  (loop [children (enumeration-seq (.children root))]
    (let [child (first children)]
      (cond
        (empty? children)
        nil

        (= id (.getUserObject child))
        child

        :else
        (let [found-child (find-header-node child id)]
          (if (some? found-child)
            found-child
            (recur (rest children)))
          ))
      )))