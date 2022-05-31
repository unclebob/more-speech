(ns more-speech.ui.swing.article-tree-util
  (:use [seesaw core])
  (:require [more-speech.ui.swing.ui-context :refer :all])
  (:import (java.util Collections)
           (javax.swing.tree DefaultMutableTreeNode TreePath)))

(defn find-chronological-insertion-point
  "Searches first level of the header tree (not including any of the children) for
  the best place to chronologically insert a new event.  Returns the index."
  [root event-id event-map]
  (let [comparator (fn [node1 node2]
                     (let [v1 (->> node1 .getUserObject (get event-map) :created-at)
                           v2 (->> node2 .getUserObject (get event-map) :created-at)]
                       (compare v2 v1)))
        children (enumeration-seq (.children root))
        dummy-node (DefaultMutableTreeNode. event-id)
        insertion-point (if (nil? children)
                          0
                          (Collections/binarySearch children dummy-node comparator))]
    (if (neg? insertion-point)
      (- (inc insertion-point))                             ;undo weird 'not-found' math of binarySearch
      insertion-point)
    )
  )

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

(defn select-tree-node [tree node]
  (let [tree-path (TreePath. (.getPath ^DefaultMutableTreeNode node))]
    (.setSelectionPath tree tree-path)
    (.scrollPathToVisible tree tree-path)))

(defn id-click [ui-context id]
  (let [frame (:frame @ui-context)
        tab-panel (select frame [:#header-tab-panel])
        selected-tab (:selected-tab @ui-context)
        tab-selector (keyword (str "#" (name selected-tab)))
        tree (select frame [tab-selector])
        model (config tree :model)
        root-node (.getRoot model)
        node (find-header-node root-node id)]
    (if (some? node)
      (select-tree-node tree node)
      (let [tree (select frame [:#all])
            model (config tree :model)
            root-node (.getRoot model)
            node (find-header-node root-node id)]
        (when (some? node)
          (selection! tab-panel "all")
          (select-tree-node tree node))
        )
      )))

(declare adjust-back-count display-event)

(defn go-back-by [n]
  (let [event-context (:event-context @ui-context)
        event-history (:event-history @event-context)]
    (when-not (empty? event-history)
      (swap! event-context adjust-back-count n)
      (let [back-count (:back-count @event-context)
            event-position (- (count event-history) back-count 1)
            [tab-id event-id] (nth event-history event-position)]
        (display-event tab-id event-id)))))

(defn adjust-back-count [event-data n]
  (let [event-history (:event-history event-data)
        back-count (-> (:back-count event-data) (+ n) (max 0) (min (dec (count event-history))))]
    (assoc event-data :back-count back-count :backing-up true))
  )

(defn display-event [tab-id event-id]
  (let [frame (:frame @ui-context)
        tab-panel (select frame [:#header-tab-panel])
        tab-selector (keyword (str "#" (name tab-id)))
        tree (select frame [tab-selector])
        model (config tree :model)
        root-node (.getRoot model)
        node (find-header-node root-node event-id)]
    (when (some? node)
      (selection! tab-panel (name tab-id))
      (select-tree-node tree node)
      )))