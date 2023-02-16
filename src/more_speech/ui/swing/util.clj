(ns more-speech.ui.swing.util
  (:require [more-speech.mem :refer :all]
            [clojure.core.async :as async]
            [more-speech.db.gateway :as gateway]
            [more-speech.config :as config]
            [more-speech.nostr.event-handlers :as event-handlers]
            [more-speech.ui.swing.article-tree-util :as at-util])
  (:use [seesaw core])
  (:import (java.awt.datatransfer StringSelection)))

(defn clear-popup [popup]
  (while (not (empty? (.getSubElements popup)))
    (.remove popup 0)))


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

(defn delete-tab-from-tabs-list [tab-name]
  (loop [tabs-list (get-mem :tabs-list)
         new-tabs-list []]
    (cond
      (empty? tabs-list)
      (set-mem :tabs-list new-tabs-list)

      (= tab-name (:name (first tabs-list)))
      (recur (rest tabs-list) new-tabs-list)

      :else
      (recur (rest tabs-list)
             (conj new-tabs-list (first tabs-list))))))

(defn add-tab-to-tabs-list [tab-name]
  (let [tabs-list (get-mem :tabs-list)
        new-tabs-list (conj tabs-list {:name tab-name :selected [] :blocked []})]
    (set-mem :tabs-list new-tabs-list)))


(defn add-id-to-tab [tab-name key id]
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

(defn- get-clipboard []
  (.getSystemClipboard (java.awt.Toolkit/getDefaultToolkit)))

(defn copy-to-clipboard [text _e]
  (let [selection (StringSelection. text)]
    (.setContents (get-clipboard) selection selection)))

(defn load-event [id]
  (when-not (contains? (get-mem :node-map) id)
    (let [event (gateway/get-event (config/get-db) id)
          handler (get-mem :event-handler)]
      (when (some? event)
        (event-handlers/immediate-add-text-event handler event)))))

(defn select-event [id]
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
      (let [tree (select frame [(keyword (str "#" (get-tab-index "all")))])
            model (config tree :model)
            root-node (.getRoot model)
            node (at-util/find-header-node root-node id)]
        (when (some? node)
          (select-tab "all")
          (at-util/select-tree-node tree node))))))
