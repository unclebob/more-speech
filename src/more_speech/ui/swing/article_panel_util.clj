(ns more-speech.ui.swing.article-panel-util
  (:use [seesaw core])
  (:require [more-speech.mem :refer :all]
            [more-speech.nostr.event-handlers :as event-handlers]
            [more-speech.ui.swing.util :as util]
            [more-speech.db.gateway :as gateway]
            [more-speech.config :refer [get-db]]
            [more-speech.ui.swing.article-tree-util :as at-util]))

(defn load-event [id]
  (when-not (contains? (get-mem :node-map) id)
    (let [event (gateway/get-event (get-db) id)
          handler (get-mem :event-handler)]
      (when (some? event)
        (event-handlers/immediate-add-text-event handler event)))))

(defn id-click [id]
  (load-event id)
  (let [frame (get-mem :frame)
        tab-index (get-mem :selected-tab)
        tab-selector (keyword (str "#" tab-index))
        tree (select frame [tab-selector])
        model (config tree :model)
        root-node (.getRoot model)
        node (at-util/find-header-node root-node id)]
    (if (some? node)
      (at-util/select-tree-node tree node)
      (let [tree (select frame [(keyword (str "#" (util/get-tab-index "all")))])
            model (config tree :model)
            root-node (.getRoot model)
            node (at-util/find-header-node root-node id)]
        (when (some? node)
          (util/select-tab "all")
          (at-util/select-tree-node tree node))))))

(defn display-event [tab-index event-id]
  (let [frame (get-mem :frame)
        tab-selector (keyword (str "#" tab-index))
        tree (select frame [tab-selector])
        model (config tree :model)
        root-node (.getRoot model)
        node (at-util/find-header-node root-node event-id)]
    (when (some? node)
      (util/select-tab tab-index)
      (at-util/select-tree-node tree node))))

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
