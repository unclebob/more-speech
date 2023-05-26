(ns more-speech.ui.swing.article-tree
  (:require
    [more-speech.mem :refer :all]
    [more-speech.nostr.events :as events]
    [more-speech.ui.swing.article-tree-util :refer :all]
    [more-speech.ui.swing.tabs :as tabs])
  (:use (seesaw [color] [core] [font] [tree]))
  (:import (javax.swing.tree DefaultMutableTreeNode)))

(defn add-orphaned-reference [referent id]
  (letfn [(update-reference [orphaned-references referent]
            (if (empty? (get orphaned-references referent))
              (assoc orphaned-references referent #{id})
              (update orphaned-references referent conj id)))]
    (update-mem :orphaned-references update-reference referent)))

;; at the moment an event can appear in several places in the tree.
;; it can be in the reply chain of an event, and it can stand alone.
;; The node-map holds the list of nodes that correspond to the id of
;; an event.

(defn node-contains? [node id]
  (loop [child-indices (range (.getChildCount node))]
    (if (empty? child-indices)
      false
      (let [child-index (first child-indices)
            child (.getChildAt node child-index)
            child-id (.getUserObject child)]
        (if (= child-id id)
          true
          (recur (rest child-indices)))))))

(defn add-this-node-to-reference-nodes [reference-nodes this-id]
  (loop [nodes reference-nodes]
    (if (empty? nodes)
      nil
      (when-not (node-contains? (first nodes) this-id)
        (let [node (first nodes)
              child (DefaultMutableTreeNode. this-id)]
          (.add ^DefaultMutableTreeNode node child)
          (update-mem [:node-map this-id] conj child)
          (recur (rest nodes)))))))

(defn add-references [event]
  (let [[_ _ referent] (events/get-references event)
        id (:id event)]
    (if (nil? referent)
      nil
      (let [nodes (get-mem [:node-map referent])]
        (if (empty? nodes)
          (add-orphaned-reference referent id)
          (add-this-node-to-reference-nodes nodes id))))))

(defn copy-node [node]
  (loop [copied-node (DefaultMutableTreeNode. (.getUserObject node))
         children (enumeration-seq (.children node))]
    (if (empty? children)
      copied-node
      (let [child (copy-node (first children))]
        (.add copied-node child)
        (recur copied-node (rest children))))))

(defn build-orphan-node [orphan-id]
  (let [node-map (get-mem :node-map)
        orphan-nodes (get node-map orphan-id)]
    (if (empty? orphan-nodes)
      nil
      (copy-node (first orphan-nodes)))))

(defn resolve-any-orphans [parent-id]
  (let [parent-nodes (get-mem [:node-map parent-id])
        orphan-set (get-mem [:orphaned-references parent-id])]
    (if (empty? orphan-set)
      nil
      (do
        (loop [orphan-set orphan-set]
          (if (empty? orphan-set)
            nil
            (let [orphan-id (first orphan-set)]
              (doseq [parent-node parent-nodes]
                (let [orphan-node (build-orphan-node orphan-id)]
                  (when (some? orphan-node)
                    (.add ^DefaultMutableTreeNode parent-node orphan-node)
                    (update-mem [:node-map orphan-id] conj orphan-node))))
              (recur (rest orphan-set)))))
        (set-mem [:orphaned-references parent-id] #{})))))

(defn add-event [event]
  (when (empty? (get-mem [:node-map (:id event)]))
    (loop [tabs (get-mem :tabs-list)]
      (if (empty? tabs)
        nil
        (do
          (tabs/add-event-to-tab (first tabs) event)
          (recur (rest tabs)))))
    ; (add-references event)
    ;(let [event-id (bigint (:id event))]
    ;  (resolve-any-orphans event-id))
    ))


