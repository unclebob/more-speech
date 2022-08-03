(ns more-speech.ui.swing.article-tree
  (:require
    [more-speech.ui.swing.article-tree-util :refer :all]
    [more-speech.nostr.events :as events]
    [more-speech.ui.formatters :as formatters]
    [more-speech.config :as config]
    [more-speech.ui.swing.article-panel :as article-panel]
    [more-speech.ui.swing.ui-context :refer :all]
    [clojure.set :as set]
    [more-speech.nostr.util :as util]
    [more-speech.ui.swing.util :as swing-util]
    [more-speech.nostr.trust-updater :as trust-updater])
  (:use [seesaw core font tree])
  (:import (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel TreePath)))

(declare render-event node-selected mouse-pressed)

(defn make-header-tree [tab-index]
  (let [header-tree (tree :renderer render-event
                          :root-visible? false
                          :expands-selected-paths? true
                          :model (DefaultTreeModel. (DefaultMutableTreeNode. "Root")))]
    (listen header-tree :selection (partial node-selected tab-index))
    (listen header-tree :mouse-pressed mouse-pressed)
    header-tree))

(declare get-info
         add-author-to-tab
         block-author-from-tab
         add-article-to-tab
         block-article-from-tab
         trust-this-author)


(defn mouse-pressed [e]
  (when (.isPopupTrigger e)
    (let [tree (.getComponent e)
          path (.getPathForLocation tree (.getX e) (.getY e))
          node (.getLastPathComponent path)
          event-id (.getUserObject ^DefaultMutableTreeNode node)
          event-context (:event-context @ui-context)
          event-map (:text-event-map @event-context)
          event (get event-map event-id)
          public-key (:pubkey event)
          tab-names (vec (remove #(= "all" %) (map :name (:tabs-list @event-context))))
          tab-names (conj tab-names "<new-tab>")
          add-author-actions (map #(action :name % :handler (partial add-author-to-tab public-key %)) tab-names)
          block-author-actions (map #(action :name % :handler (partial block-author-from-tab public-key %)) tab-names)
          add-article-actions (map #(action :name % :handler (partial add-article-to-tab event-id %)) tab-names)
          block-article-actions (map #(action :name % :handler (partial block-article-from-tab event-id %)) tab-names)
          p (popup :items [(action :name "Get info..."
                                   :handler (partial get-info event))
                           (action :name "Trust this author..."
                                   :handler (partial trust-this-author event))
                           (menu :text "Add author to tab" :items add-author-actions)
                           (menu :text "Block author from tab" :items block-author-actions)
                           (menu :text "Add article to tab" :items add-article-actions)
                           (menu :text "Block article from tab" :items block-article-actions)
                           ])]
      (.show p (to-widget e) (.x (.getPoint e)) (.y (.getPoint e))))
    ))

(defn if-new-tab [tab-name]
  (if (= tab-name "<new-tab>")
    (let [new-tab-name (input "New tab name:")
          new-tab-name (swing-util/unduplicate-tab-name new-tab-name)]
      (if (seq new-tab-name)
        (do (swing-util/add-tab-to-tabs-list new-tab-name)
            new-tab-name)
        nil))
    tab-name))

(defn add-author-to-tab [public-key tab-name _e]
  (when-let [tab-name (if-new-tab tab-name)]
    (swing-util/add-id-to-tab tab-name :selected public-key)
    (swing-util/relaunch)))

(defn block-author-from-tab [public-key tab-name _e]
  (when-let [tab-name (if-new-tab tab-name)]
    (swing-util/add-id-to-tab tab-name :blocked public-key)
    (swing-util/relaunch)))

(defn add-article-to-tab [event-id tab-name _e]
  (when-let [tab-name (if-new-tab tab-name)]
    (swing-util/add-id-to-tab tab-name :selected event-id)
    (swing-util/relaunch)))

(defn block-article-from-tab [event-id tab-name _e]
  (when-let [tab-name (if-new-tab tab-name)]
    (swing-util/add-id-to-tab tab-name :blocked event-id)
    (swing-util/relaunch)))

(defn get-info [event _e]
  (alert
    (with-out-str
      (clojure.pprint/pprint event))))

(defn trust-this-author [event _e]
  (let [his-pubkey (:pubkey event)
        event-context (:event-context @ui-context)
        profiles (:profiles @event-context)
        profile (get profiles his-pubkey)
        petname (input "Name this author"
                       :value (:name profile)
                       :title (str "Entrust " (formatters/abbreviate (util/num32->hex-string his-pubkey) 10)))]
    (when (some? petname)
      (trust-updater/entrust-and-send his-pubkey petname))))

(defn select-article
  [tab-index selected-node]
  (let [selected-id (.getUserObject selected-node)
        event-context (:event-context @ui-context)]
    (swap! event-context events/select-event tab-index selected-id)
    (article-panel/load-article-info selected-id)))

(defn node-selected [tab-index e]
  (let [selected-node (last (selection e))]
    (when (and
            (some? selected-node)
            (instance? DefaultMutableTreeNode selected-node))
      (select-article tab-index selected-node))))

(defn render-event [widget info]
  (if (seqable? (:value info))
    (text! widget "Articles")
    (let [event-context (:event-context @ui-context)
          event-state @event-context
          event-map (:text-event-map event-state)
          node (:value info)
          event-id (.getUserObject node)
          event (get event-map event-id)
          read? (contains? (:read-event-ids @event-context) event-id)
          font (if read? config/default-font config/bold-font)]
      (config! widget :font font)
      (text! widget (formatters/format-header event)))))

(declare add-references resolve-any-orphans)

(defn should-add-event? [filters event]
  (let [selected (:selected filters)
        blocked (:blocked filters)
        [root _mentions _referent] (events/get-references event)
        tags (:tags event)
        ptags (filter #(= :p (first %)) tags)
        pubkey-citings (map #(util/hex-string->num (second %)) ptags)]
    (and
      (or
        (empty? selected)
        (some #(= % (:pubkey event)) selected)
        (some #(= % (:id event)) selected)
        (some #(= % root) selected)
        (not-empty (set/intersection (set pubkey-citings) (set selected))))
      (not
        (or
          (some #(= % (:pubkey event)) blocked)
          (some #(= % (:id event)) blocked))))
    )
  )

(defn add-event [event]
  (let [frame (:frame @ui-context)
        event-state @(:event-context @ui-context)
        event-map (:text-event-map event-state)
        event-id (:id event)]
    (loop [tabs (:tabs-list event-state)
           index 0]
      (if (empty? tabs)
        nil
        (let [tree-id (keyword (str "#" index))
              tree (select frame [tree-id])]
          (when (should-add-event? (first tabs) event)
            (let [model (config tree :model)
                  root (.getRoot model)
                  insertion-point (find-chronological-insertion-point root event-id event-map)
                  child (DefaultMutableTreeNode. event-id)]
              (.insertNodeInto model child root insertion-point)
              (.makeVisible tree (TreePath. (.getPath child)))
              (swap! ui-context update-in [:node-map event-id] conj child)
              ))
          (recur (rest tabs) (inc index)))))
    (add-references event)
    (resolve-any-orphans event-id)))

(declare build-orphan-node)

(defn resolve-any-orphans [parent-id]
  (let [parent-nodes (get-in @ui-context [:node-map parent-id])
        orphan-set (get-in @ui-context [:orphaned-references parent-id])]
    (if (empty? orphan-set)
      nil
      (do
        (loop [orphan-set orphan-set]
          (if (empty? orphan-set)
            nil
            (let [orphan-id (first orphan-set)]
              (doseq [parent-node parent-nodes]
                (let [orphan-node (build-orphan-node orphan-id)]
                  (.add ^DefaultMutableTreeNode parent-node orphan-node)
                  (swap! ui-context update-in [:node-map orphan-id] conj orphan-node)))
              (recur (rest orphan-set)))))
        (swap! ui-context assoc-in [:orphaned-references parent-id] #{})))))

(declare copy-node)

(defn build-orphan-node [orphan-id]
  (let [node-map (:node-map @ui-context)
        orphan-nodes (get node-map orphan-id)]
    (copy-node (first orphan-nodes))))

(defn copy-node [node]
  (loop [copied-node (DefaultMutableTreeNode. (.getUserObject node))
         children (enumeration-seq (.children node))]
    (if (empty? children)
      copied-node
      (let [child (copy-node (first children))]
        (.add copied-node child)
        (recur copied-node (rest children))))))

;; at the moment an event can appear in several places in the tree.
;; it can be in the reply chain of an event, and it can stand alone.
;; The node-map holds the list of nodes that correspond to the id of
;; an event.

(declare add-orphaned-reference add-this-node-to-reference-nodes)

(defn add-references [event]
  (let [[_ _ referent] (events/get-references event)
        id (:id event)]
    (if (nil? referent)
      nil
      (let [nodes (get-in @ui-context [:node-map referent])]
        (if (empty? nodes)
          (add-orphaned-reference referent id)
          (add-this-node-to-reference-nodes nodes id)
          )))))


(defn add-orphaned-reference [referent id]
  (swap! ui-context
         (fn [ui-context]
           (if (empty? (get-in ui-context [:orphaned-references referent]))
             (assoc-in ui-context [:orphaned-references referent] #{id})
             (update-in ui-context [:orphaned-references referent] conj id)))))

(defn node-contains? [node id]
  (loop [child-indices (range (.getChildCount node))]
    (if (empty? child-indices)
      false
      (let [child-index (first child-indices)
            child (.getChildAt node child-index)
            child-id (.getUserObject child)]
        (if (= child-id id)
          true
          (recur (rest child-indices)))))
    ))

(defn add-this-node-to-reference-nodes [reference-nodes this-id]
  (loop [nodes reference-nodes]
    (if (empty? nodes)
      nil
      (when-not (node-contains? (first nodes) this-id)
        (let [node (first nodes)
              child (DefaultMutableTreeNode. this-id)]
          (.add ^DefaultMutableTreeNode node child)
          (swap! ui-context update-in [:node-map this-id] conj child)
          (recur (rest nodes)))))))