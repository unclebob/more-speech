(ns more-speech.ui.swing.tabs
  (:use [seesaw core font tree])
  (:require [more-speech.ui.swing.util :as swing-util]
            [more-speech.nostr.util :as util]
            [more-speech.mem :refer :all]
            [more-speech.config :refer [get-db]]
            [more-speech.ui.formatters :as formatters]
            [more-speech.user-configuration :as uconfig]
            [more-speech.db.gateway :as gateway]
            [more-speech.nostr.events :as events]
            [more-speech.ui.swing.article-panel :as article-panel]
            [more-speech.ui.swing.article-tree-util :as at-util]
            [clojure.set :as set]
            [more-speech.nostr.trust-updater :as trust-updater]
            [more-speech.ui.swing.edit-window :as edit-window]
            [more-speech.ui.formatter-util :as f-util])
  (:use [seesaw core]
        [seesaw core font tree color])
  (:import (javax.swing.tree DefaultTreeModel DefaultMutableTreeNode TreePath)))

(declare mouse-pressed tab-menu)

(defn render-event [widget info]
  (if (seqable? (:value info))
    (text! widget "Articles")
    (let [node (:value info)
          event-id (.getUserObject node)
          event-id (if (number? event-id) event-id 0)       ;dummy event-idi
          event (gateway/get-event (get-db) event-id)
          read? (:read event)
          font (if read? (uconfig/get-default-font) (uconfig/get-bold-font))
          color (if (= (:kind event) 4) :blue :black)]
      (config! widget :font font :foreground color)
      (text! widget (formatters/format-header event)))))

(defn select-article
  [tab-index selected-node]
  (let [selected-id (.getUserObject selected-node)]
    (events/select-event tab-index selected-id)
    (article-panel/load-article-info selected-id)))

(defn node-selected [tab-index e]
  (let [selected-node (last (selection e))]
    (when (and
            (some? selected-node)
            (instance? DefaultMutableTreeNode selected-node))
      (select-article tab-index selected-node))))

(defn make-header-tree [tab-index]
  (let [header-tree (tree :renderer render-event
                          :root-visible? false
                          :expands-selected-paths? true
                          :model (DefaultTreeModel. (DefaultMutableTreeNode. 0)))
        renderer (.getCellRenderer header-tree)
        _ (.setBackgroundSelectionColor renderer (color :azure))]
    (listen header-tree :selection (partial node-selected tab-index))
    (listen header-tree :mouse-pressed mouse-pressed)
    header-tree))

(defn make-tab-data [tab-desc tab-index]
  (let [tab-name (:name tab-desc)
        header-tree (make-header-tree tab-index)
        _ (config! header-tree
                   :user-data tab-index
                   :id (keyword (str tab-index)))
        tab-label (label :text tab-name :user-data tab-index)
        _ (listen tab-label :mouse-pressed tab-menu)
        tab-content (scrollable header-tree)]
    {:title tab-label
     :content tab-content}))

(defn add-tab-to-tree [tab-data]
  (let [frame (get-mem :frame)
        tab-panel (select frame [:#header-tab-panel])
        component (:content tab-data)
        title (:title tab-data)]
    (.addTab tab-panel "" component)
    (.setTabComponentAt tab-panel (dec (.getTabCount tab-panel)) title)))

(defn if-new-tab [tab-name]
  (if (= tab-name "<new-tab>")
    (let [new-tab-name (input "New tab name:")
          new-tab-name (swing-util/unduplicate-tab-name new-tab-name)]
      (if (seq new-tab-name)
        (do (swing-util/add-tab-to-tabs-list new-tab-name)
            (let [new-tab {:name new-tab-name :selected [] :blocked []}
                  tab-index (swing-util/get-tab-index new-tab-name)
                  tab-data (make-tab-data new-tab tab-index)]
              (add-tab-to-tree tab-data))
            new-tab-name)
        nil))
    tab-name))

(defn valid-ptag? [ptag]
  (= 64 (count (second ptag))))

(defn should-add-event? [filters event]
  (let [selected (:selected filters)
        blocked (:blocked filters)
        [root _mentions _referent] (events/get-references event)
        tags (:tags event)
        ptags (filter #(= :p (first %)) tags)
        ptags (filter valid-ptag? ptags)
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
          (some #(= % (:id event)) blocked))))))

(defn add-event-to-tab-tree [tree event-id]
  (let [model (config tree :model)
        root (.getRoot model)
        insertion-point (at-util/find-chronological-insertion-point root event-id)
        child (DefaultMutableTreeNode. event-id)]
    (.insertNodeInto model child root insertion-point)
    (.makeVisible tree (TreePath. (.getPath child)))
    (update-mem [:node-map event-id] conj child)))

(defn add-event-to-tab [tab event]
  (when (should-add-event? tab event)
    (let [tab-index (swing-util/get-tab-index (:name tab))
          tree-id (keyword (str "#" tab-index))
          frame (get-mem :frame)
          tree (select frame [tree-id])
          event-id (bigint (:id event))]
      (add-event-to-tab-tree tree event-id))))

(defn add-author-to-tab [public-key tab-name _e]
  (when-let [tab-name (if-new-tab tab-name)]
    (swing-util/add-id-to-tab tab-name :selected public-key)
    (let [now (util/get-now)
          since (- now 86400)
          tab (swing-util/get-tab-by-name tab-name)
          ids (gateway/get-ids-by-author-since (get-db) public-key since)]
      (swing-util/select-tab tab-name)
      (prn 'adding (count ids) 'events)
      (doseq [id ids]
        (add-event-to-tab tab (gateway/get-event (get-db) id))))))

(defn block-author-from-tab [public-key tab-name _e]
  (when-let [tab-name (if-new-tab tab-name)]
    (swing-util/add-id-to-tab tab-name :blocked public-key)))

(defn add-article-to-tab [event-id tab-name _e]
  (when-let [tab-name (if-new-tab tab-name)]
    (let [root-of-thread (events/get-root-of-thread event-id)]
      (swing-util/add-id-to-tab tab-name :selected root-of-thread)
      (let [now (util/get-now)
            since (- now 86400)
            tab (swing-util/get-tab-by-name tab-name)
            ids (gateway/get-ids-that-cite-since (get-db) root-of-thread since)
            ids (set (concat [root-of-thread event-id] ids))]
        (swing-util/select-tab tab-name)
        (prn 'adding (count ids) 'events)
        (doseq [id ids]
          (add-event-to-tab tab (gateway/get-event (get-db) id)))))))

(defn block-article-from-tab [event-id tab-name _e]
  (when-let [tab-name (if-new-tab tab-name)]
    (swing-util/add-id-to-tab tab-name :blocked event-id)))

(defn delete-tab [tab-label _e]
  (let [tab-name (config tab-label :text)
        tab-index (swing-util/get-tab-index tab-name)]
    (when (confirm (format "Confirm delete %s" tab-name))
      (swing-util/delete-tab-from-tabs-list tab-name)
      (let [frame (get-mem :frame)
            tab-panel (select frame [:#header-tab-panel])]
        (.remove tab-panel tab-index)))))

(defn new-tab [_e]
  (let [new-tab-name (input "New tab name:")
        new-tab-name (swing-util/unduplicate-tab-name new-tab-name)]
    (when (seq new-tab-name)
      (swing-util/add-tab-to-tabs-list new-tab-name))))

(defn change-tab-name [tab-label _e]
  (let [tab-name (config tab-label :text)
        new-name (input (format "rename %s to:" tab-name))
        new-name (swing-util/unduplicate-tab-name new-name)
        frame (get-mem :frame)
        tab-panel (select frame [:#header-tab-panel])
        tab-index (config tab-label :user-data)]
    (when (and (some? tab-index) (seq new-name))
      (let [label (.getTabComponentAt tab-panel tab-index)]
        (config! label :text new-name)
        (swing-util/change-tabs-list-name tab-name new-name)))))

(defn make-tabs []
  (loop [tabs-list (get-mem :tabs-list)
         header-tree-tabs []
         tab-index 0]
    (if (empty? tabs-list)
      header-tree-tabs
      (let [tab-data (make-tab-data (first tabs-list) tab-index)]
        (recur (rest tabs-list)
               (conj header-tree-tabs tab-data)
               (inc tab-index))))))

(defn ensure-tab-list-has-all [tab-list]
  (if (some #(= "all" (:name %)) tab-list)
    tab-list
    (conj tab-list {:name "all" :selected [] :blocked []})))

(defn get-info [event _e]
  (alert
    (with-out-str
      (clojure.pprint/pprint (formatters/hexify-event event)))))

(defn trust-this-author [event _e]
  (let [his-pubkey (:pubkey event)
        profile (gateway/get-profile (get-db) his-pubkey)
        petname (input "Name this author"
                       :value (:name profile)
                       :title (str "Entrust " (f-util/abbreviate (util/num32->hex-string his-pubkey) 10)))]
    (when (some? petname)
      (trust-updater/entrust-and-send his-pubkey petname))))

(defn dm-author [event _e]
  (let [pubkey (:pubkey event)
        content (str "D @" (formatters/get-best-name pubkey) " ")]
    (edit-window/make-edit-window :send content)
    )
  )

;------DECLARED

(defn mouse-pressed [e]
  (when (.isPopupTrigger e)
    (let [tree (.getComponent e)
          path (.getPathForLocation tree (.getX e) (.getY e))
          node (.getLastPathComponent path)
          event-id (.getUserObject ^DefaultMutableTreeNode node)
          event (gateway/get-event (get-db) event-id)
          public-key (:pubkey event)
          tab-names (vec (remove #(= "all" %) (map :name (get-mem :tabs-list))))
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
                           (action :name "DM author..." :handler (partial dm-author event))
                           ])]
      (.show p (to-widget e) (.x (.getPoint e)) (.y (.getPoint e))))))

(defn tab-menu [e]
  (let [tab-label (.getComponent e)
        tab-name (config tab-label :text)
        tab-index (config tab-label :user-data)
        isAll? (= "all" tab-name)
        p (popup :items [(action :name "Change name..."
                                 :handler (partial change-tab-name tab-label)
                                 :enabled? (not isAll?))
                         (action :name (str "Delete " tab-name "...")
                                 :handler (partial delete-tab tab-label)
                                 :enabled? (not isAll?))
                         (action :name "New tab..."
                                 :handler new-tab
                                 :enabled? true)])]
    (if (.isPopupTrigger e)
      (.show p (to-widget e) (.x (.getPoint e)) (.y (.getPoint e)))
      (swing-util/select-tab tab-index))))

