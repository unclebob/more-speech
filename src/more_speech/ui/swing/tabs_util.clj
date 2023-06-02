(ns more-speech.ui.swing.tabs-util
  (:use [seesaw core])
  (:require [more-speech.mem :refer :all]
            [more-speech.db.gateway :as gateway]
            [more-speech.config :refer [get-db]])
  (:import (java.util Collections)
           (javax.swing.tree DefaultMutableTreeNode TreePath)))

(defn find-chronological-insertion-point
  "Searches first level of the header tree (not including any of the children) for
  the best place to chronologically insert a new event.  Returns the index."
  [root event-id]
  (let [comparator (fn [node1 node2]
                     (let [v1 (->> node1 .getUserObject (gateway/get-event (get-db)) :created-at)
                           v2 (->> node2 .getUserObject (gateway/get-event (get-db)) :created-at)]
                       (compare v2 v1)))
        children (enumeration-seq (.children root))
        dummy-node (DefaultMutableTreeNode. event-id)
        insertion-point (if (nil? children)
                          0
                          (Collections/binarySearch children dummy-node comparator))]
    (if (neg? insertion-point)
      (- (inc insertion-point))                             ;undo weird 'not-found' math of binarySearch
      insertion-point)))

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
            (recur (rest children))))))))

(defn find-header-node [root id]
  (search-for-node root #(= (bigint id) (bigint %))))

(defn select-tree-node [tree node]
  (let [tree-path (TreePath. (.getPath ^DefaultMutableTreeNode node))]
    (.clearSelection tree)
    (.setSelectionPath tree tree-path)
    (.scrollPathToVisible tree tree-path)))



