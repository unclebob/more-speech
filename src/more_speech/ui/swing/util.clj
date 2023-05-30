(ns more-speech.ui.swing.util
  (:require [clojure.core.async :as async]
            [more-speech.config :as config]
            [more-speech.db.gateway :as gateway]
            [more-speech.mem :refer :all]
            [more-speech.nostr.event-dispatcher :as event-handlers]
            [more-speech.nostr.util :as util]
            [more-speech.ui.swing.article-tree-util :as at-util])
  (:use (seesaw [core])))

(defn clear-popup [popup]
  (while (not (empty? (.getSubElements popup)))
    (.remove popup 0)))

(defn get-tab-by-name [name]
  (first (filter #(= name (:name %)) (get-mem :tabs-list))))

(defn get-tab-index [name]
  (loop [tab-list (get-mem :tabs-list)
         index 0]
    (cond
      (empty? tab-list) nil
      (= name (:name (first tab-list))) index
      :else (recur (rest tab-list) (inc index)))))

(defn select-tab
  "Select tab by name or index"
  [tab-selector]
  (let [frame (get-mem :frame)
        tabbed-panel (select frame [:#header-tab-panel])
        tab-index (if (number? tab-selector)
                    tab-selector
                    (get-tab-index tab-selector))]
    (selection! tabbed-panel tab-index)))

(defn change-tabs-list-name [old-name new-name]
  (loop [tabs-list (get-mem :tabs-list)
         new-tabs-list []]
    (cond
      (empty? tabs-list)
      (set-mem :tabs-list new-tabs-list)

      (= old-name (:name (first tabs-list)))
      (recur (rest tabs-list)
             (conj new-tabs-list (assoc (first tabs-list) :name new-name)))

      :else
      (recur (rest tabs-list)
             (conj new-tabs-list (first tabs-list))))))

(defn delete-tab-from-tabs-list [tabs-list tab-name]
  (loop [tabs-list tabs-list
         new-tabs-list []]
    (cond
      (empty? tabs-list)
      new-tabs-list

      (= tab-name (:name (first tabs-list)))
      (recur (rest tabs-list) new-tabs-list)

      :else
      (recur (rest tabs-list)
             (conj new-tabs-list (first tabs-list))))))

(defn add-tab-to-tabs-list [tab-name]
  (let [tabs-list (get-mem :tabs-list)
        tab-desc {:name tab-name :selected [:empty] :blocked []}
        new-tabs-list (conj tabs-list tab-desc)]
    (set-mem :tabs-list new-tabs-list)
    tab-desc))

(defn remove-id-from-tab [tab-name key id]
  (loop [tabs-list (get-mem :tabs-list)
         new-tabs-list []]
    (cond
      (empty? tabs-list)
      (set-mem :tabs-list new-tabs-list)

      (= tab-name (:name (first tabs-list)))
      (let [tab-descriptor (first tabs-list)
            id-list (get tab-descriptor key [])
            id-list (remove #(= id %) id-list)]
        (recur (rest tabs-list) (conj new-tabs-list (assoc tab-descriptor key id-list))))

      :else
      (recur (rest tabs-list)
             (conj new-tabs-list (first tabs-list))))))


(defn add-filter-to-tab [tab-name key id]
  (loop [tabs-list (get-mem :tabs-list)
         new-tabs-list []]
    (cond
      (empty? tabs-list)
      (set-mem :tabs-list new-tabs-list)

      (= tab-name (:name (first tabs-list)))
      (let [tab-descriptor (first tabs-list)
            id-list (get tab-descriptor key [])
            id-list (conj id-list id)]
        (recur (rest tabs-list) (conj new-tabs-list (assoc tab-descriptor key id-list))))

      :else
      (recur (rest tabs-list)
             (conj new-tabs-list (first tabs-list))))))

(defn unduplicate-tab-name [tab-name]
  (if (some? (get-tab-index tab-name))
    (str tab-name "-" (rand-int 1000000))
    tab-name))

(defn relaunch []
  (let [send-chan (get-mem :send-chan)]
    (future (async/>!! send-chan [:relaunch]))))

(defn copy-to-clipboard [text _e]
  (util/copy-to-clipboard text))

(defn load-event [id]
  (let [event (gateway/get-event (config/get-db) id)
        handler (get-mem :event-handler)]
    (when (some? event)
      (event-handlers/immediate-add-text-event handler event))))

(defn get-node [tab-name id]
  (let [tree (get-mem [:tab-tree-map tab-name])]
    (if (some? tree)
      (let [model (config tree :model)
            root-node (.getRoot model)]
        (at-util/find-header-node root-node id))
      nil)
    ))

(defn select-event [id]
  (load-event id)
  (let [tab-index (get-mem :selected-tab)
        tabs-list (get-mem :tabs-list)
        tab-data (get tabs-list tab-index)
        tab-name (:name tab-data)
        tree (get-mem [:tab-tree-map tab-name])
        node (get-node tab-name id)]
    (if (some? node)
      (at-util/select-tree-node tree node)
      (loop [tabs (get-mem :tabs-list)]
        (if (empty? tabs)
          nil
          (let [tab-name (:name (first tabs))
                tree (get-mem [:tab-tree-map tab-name])
                model (config tree :model)
                root-node (.getRoot model)
                node (at-util/find-header-node root-node id)]
            (if (some? node)
              (do
                (select-tab tab-name)
                (at-util/select-tree-node tree node))
              (recur (rest tabs)))))))))
