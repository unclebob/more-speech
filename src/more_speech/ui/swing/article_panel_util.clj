(ns more-speech.ui.swing.article-panel-util
  (:use [seesaw core])
  (:require [more-speech.mem :refer :all]
            [more-speech.ui.swing.util :as util]
            [more-speech.config :refer [get-db]]
            [more-speech.ui.swing.article-tree-util :as at-util]))

(defn display-event [tab-index event-id]
  (let [tabs-list (get-mem :tabs-list)
        tab-desc (get tabs-list tab-index)
        tab-name (:name tab-desc)
        tree (get-mem [:tab-tree-map tab-name])]
    (when (some? tree)
      (let [model (config tree :model)
            root-node (.getRoot model)
            node (at-util/find-header-node root-node event-id)]
        (when (some? node)
          (util/select-tab tab-index)
          (at-util/select-tree-node tree node))))))

(defn adjust-back-count [n]
  (let [event-history (get-mem :event-history)
        back-count (-> (get-mem :back-count) (+ n) (max 0) (min (dec (count event-history))))]
    (set-mem :back-count back-count)
    (set-mem :backing-up true)))

(defn go-back-by [n]
  (let [event-history (get-mem :event-history)]
    (when-not (empty? event-history)
      (adjust-back-count n)
      (let [back-count (get-mem :back-count)
            event-position (- (count event-history) back-count 1)
            [tab-index event-id] (nth event-history event-position)]
        (display-event tab-index event-id)))))
