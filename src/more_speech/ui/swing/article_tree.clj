(ns more-speech.ui.swing.article-tree
  (:require [more-speech.nostr.events :as events]
            [more-speech.ui.formatters :as formatters]
            [more-speech.ui.config :as config]
            [more-speech.nostr.util :as util])
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
          event-state @event-agent
          nicknames (:nicknames event-state)
          format-user (partial formatters/format-user-id nicknames)
          text-map (:text-event-map event-state)
          event (get text-map selected-id)
          [root-id _ referent] (events/get-references event)
          reply-to (select main-frame [:#reply-to-label])
          citing (select main-frame [:#citing-label])
          root-label (select main-frame [:#root-label])
          relays-label (select main-frame [:#relays-label])
          article-area (select main-frame [:#article-area])]
      (text! article-area (formatters/reformat-article (:content event) 80))
      (text! (select main-frame [:#author-id-label])
             (format-user (:pubkey event)))
      (text! (select main-frame [:#created-time-label])
             (formatters/format-time (:created-at event)))
      (config! (select main-frame [:#id-label])
               :user-data (:id event)
               :text (formatters/abbreviate (util/num32->hex-string (:id event)) 30))
      (if (some? referent)
        (let [replied-event (get text-map referent)]
          (text! reply-to (format-user (:pubkey replied-event)))
          (config! citing
                   :user-data referent
                   :text (formatters/abbreviate (util/num32->hex-string referent) 30)))
        (do (text! reply-to "")
            (text! citing "")))
      (if (some? root-id)
        (config! root-label
                 :user-data root-id
                 :text (formatters/abbreviate (util/num32->hex-string root-id) 30))
        (text! root-label ""))
      (text! relays-label (pr-str (count (:relays event)) (first (:relays event))))
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

(declare add-references
         find-chronological-insertion-point)
(defn add-event [ui-context event]
  (let [frame (:frame @ui-context)
        event-state @(:event-agent @ui-context)
        event-map (:text-event-map event-state)
        event-id (:id event)
        event-time (:created-at event)
        tree (select frame [:#header-tree])
        model (config tree :model)
        root (.getRoot model)
        insertion-point (find-chronological-insertion-point root event-time event-map)
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


(defn find-chronological-insertion-point [root time event-map]
  (loop [insertion-point 0
         children (enumeration-seq (.children root))]
    (cond
      (empty? children)
      insertion-point

      (>
        (->> (first children) .getUserObject (get event-map) :created-at)
        time)
      insertion-point

      :else
      (recur (inc insertion-point) (rest children)))))

(defn search-for-node [root matcher]
  (loop [children (enumeration-seq (.children root))]
    (let [child (first children)]
      (cond
        (empty? children)
        nil

        (matcher (.getUserObject child))
        child

        :else
        (let [found-child (search-for-node child matcher)]
          (if (some? found-child)
            found-child
            (recur (rest children)))
          ))
      )))

(defn find-header-node [root id]
  (search-for-node root #(= id %)))

(defn id-click [ui-context id]
  (let [frame (:frame @ui-context)
        tree (select frame [:#header-tree])
        model (config tree :model)
        root-node (.getRoot model)
        node (find-header-node root-node id)]
    (when (some? node)
      (let [tree-path (TreePath. (.getPath ^DefaultMutableTreeNode node))]
        (.setSelectionPath tree tree-path)
        (.scrollPathToVisible tree tree-path)
        ))))