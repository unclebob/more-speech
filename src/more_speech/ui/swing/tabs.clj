(ns more-speech.ui.swing.tabs
  (:require
    [clojure.set :as set]
    [more-speech.config :as config]
    [more-speech.config :refer [get-db]]
    [more-speech.db.gateway :as gateway]
    [more-speech.logger.default :refer [log-pr]]
    [more-speech.mem :refer :all]
    [more-speech.nostr.events :as events]
    [more-speech.nostr.tab-searcher :as tab-searcher]
    [more-speech.nostr.trust-updater :as trust-updater]
    [more-speech.nostr.util :as util]
    [more-speech.nostr.zaps :as zaps]
    [more-speech.ui.formatters :as formatters]
    [more-speech.ui.swing.article-panel :as article-panel]
    [more-speech.ui.swing.article-tree-util :as at-util]
    [more-speech.ui.swing.edit-window :as edit-window]
    [more-speech.ui.swing.util :as swing-util]
    [more-speech.user-configuration :as uconfig])
  (:use (seesaw [color] [core] [font] [tree]))
  (:import (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel TreePath)))

(declare article-click tab-menu)

(defn render-event [widget info]
  (if (seqable? (:value info))
    (text! widget "Notes")
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

(defn make-header-tree [tab-name]
  (let [tab-index (swing-util/get-tab-index tab-name)
        header-tree (tree :renderer render-event
                          :root-visible? false
                          :expands-selected-paths? true
                          :model (DefaultTreeModel. (DefaultMutableTreeNode. 0)))
        renderer (.getCellRenderer header-tree)
        _ (.setBackgroundSelectionColor renderer (color :azure))]
    (listen header-tree :selection (partial node-selected tab-index))
    (listen header-tree :mouse-pressed article-click)
    (set-mem [:tab-tree-map tab-name] header-tree)
    header-tree))

(defn make-search-bar [tab-name]
  (let [status-id (keyword (str tab-name "-status"))
        search-id (keyword (str tab-name "-search"))
        search-field (text :text "" :editable? true :columns 50 :id search-id)
        status-field (text :text "" :editable? false :columns 10 :id status-id)
        prev-search (label "⬆")
        next-search (label "⬇")
        search-items [(label "Find:")
                      search-field status-field
                      next-search prev-search]
        bar (flow-panel :align :left :items search-items)]
    (listen search-field :key-pressed (partial tab-searcher/search-event tab-name))
    (listen prev-search :mouse-pressed
            (partial tab-searcher/select-prev tab-name))
    (listen next-search :mouse-pressed
            (partial tab-searcher/select-next tab-name))
    bar))

(defn make-tab [tab-desc]
  (let [tab-name (:name tab-desc)
        tab-label (label :text tab-name)
        _ (listen tab-label :mouse-pressed tab-menu)
        header-tree (make-header-tree tab-name)
        scrollable-header-tree (scrollable header-tree)
        search-bar (make-search-bar tab-name)
        tab-window (top-bottom-split search-bar scrollable-header-tree)
        ]
    {:title tab-label
     :content tab-window}))

(defn make-tabs []
  (loop [tabs-list (get-mem :tabs-list)
         header-tree-tabs []]
    (if (empty? tabs-list)
      header-tree-tabs
      (let [tab-data (make-tab (first tabs-list))]
        (recur (rest tabs-list)
               (conj header-tree-tabs tab-data))))))

(defn add-tab-to-tab-panel [tab-data]
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
                  tab-data (make-tab new-tab)]
              (add-tab-to-tab-panel tab-data))
            new-tab-name)
        nil))
    tab-name))

(defn valid-ptag? [ptag]
  (= 64 (count (second ptag))))

(defn should-add-event? [filters event]
  (try
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
            (some #(= % (:id event)) blocked)))))
    (catch Exception _e
      (log-pr 1 'should-add-event? 'bad-tag event)
      false)))

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
    (let [tab-name (:name tab)
          tree (get-mem [:tab-tree-map tab-name])
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
      (log-pr 2 'adding (count ids) 'events)
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
        (log-pr 2 'adding (count ids) 'events)
        (doseq [id ids]
          (add-event-to-tab tab (gateway/get-event (get-db) id)))))))

(defn block-article-from-tab [event-id tab-name _e]
  (when-let [tab-name (if-new-tab tab-name)]
    (swing-util/add-id-to-tab tab-name :blocked event-id)))

(defn delete-tab [tab-label _e]
  (let [tab-name (config tab-label :text)
        tab-index (swing-util/get-tab-index tab-name)]
    (when (confirm (format "Confirm delete %s" tab-name))
      (update-mem :tabs-list swing-util/delete-tab-from-tabs-list tab-name)
      (let [frame (get-mem :frame)
            tab-panel (select frame [:#header-tab-panel])]
        (.remove tab-panel tab-index)))))

(defn new-tab [_e]
  (let [new-tab-name (input "New tab name:")
        new-tab-name (swing-util/unduplicate-tab-name new-tab-name)]
    (when (seq new-tab-name)
      (let [tab-desc (swing-util/add-tab-to-tabs-list new-tab-name)
            tab-data (make-tab tab-desc)]
        (add-tab-to-tab-panel tab-data)))))

(defn change-tab-name [tab-label _e]
  (let [tab-name (config tab-label :text)
        new-name (input (format "rename %s to:" tab-name))
        new-name (swing-util/unduplicate-tab-name new-name)
        frame (get-mem :frame)
        tab-panel (select frame [:#header-tab-panel])
        tab-index (swing-util/get-tab-index tab-name)]
    (when (and (some? tab-index) (seq new-name))
      (let [label (.getTabComponentAt tab-panel tab-index)
            tree (get-mem [:tab-tree-map tab-name])]
        (config! label :text new-name)
        (swing-util/change-tabs-list-name tab-name new-name)
        (set-mem [:tab-tree-map new-name] tree)
        (set-mem [:tab-tree-map tab-name] nil)
        (set-mem [:tab-search new-name] (get-mem [:tab-search tab-name]))
        (set-mem [:tab-search tab-name] nil)))))

(defn ensure-tab-list-has-all [tab-list]
  (if (some #(= "all" (:name %)) tab-list)
    tab-list
    (conj tab-list {:name "all" :selected [] :blocked []})))

(defn get-info [event _e]
  (alert
    (with-out-str
      (clojure.pprint/pprint (formatters/hexify-event event)))))

(defn dm-author [event _e]
  (let [pubkey (:pubkey event)
        content (str "D @" (formatters/get-best-name pubkey) " ")]
    (edit-window/make-edit-window :send content)))

;------DECLARED

(defn article-click [e]
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
                                   :handler (partial trust-updater/trust-author-of-this-event event))
                           (menu :text "Add author to tab" :items add-author-actions)
                           (menu :text "Block author from tab" :items block-author-actions)
                           (menu :text "Add note to tab" :items add-article-actions)
                           (menu :text "Block note from tab" :items block-article-actions)
                           (action :name "DM author..." :handler (partial dm-author event))
                           (action :name "Zap author..." :handler (partial zaps/zap-author event))
                           ])]
      (.show p (to-widget e) (.x (.getPoint e)) (.y (.getPoint e))))))

(defn tab-menu [e]
  (let [tab-label (.getComponent e)
        tab-name (config tab-label :text)
        tab-index (swing-util/get-tab-index tab-name)
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


(defn remove-node-and-children-from-node-map [id node]
  (update-mem [:node-map id] (fn [nodes] (remove #(= node %) nodes)))
  (loop [children (enumeration-seq (.children node))]
    (if (empty? children)
      nil
      (let [child (first children)
            id (.getUserObject child)]
        (remove-node-and-children-from-node-map id child)
        (recur (rest children))))))

(defn delete-last-event-from-tree-model [model]
  (let [root (.getRoot model)
        child-count (.getChildCount root)
        last-node (.getChild model root (dec child-count))
        event-id (.getUserObject last-node)]

    (.removeNodeFromParent model last-node)
    (remove-node-and-children-from-node-map event-id last-node)))

(defn delete-last-event-if-too-many [model max-nodes]
  (let [root (.getRoot model)]
    (when (> (.getChildCount root) max-nodes)
      (log-pr 2 'nodes (.getChildCount root))
      (while (> (.getChildCount root) max-nodes)
        (delete-last-event-from-tree-model model))
      (log-pr 2 'nodes-remaining (.getChildCount root)))))

(defn prune-tabs []
  (log-pr 2 'prune-tabs)
  (loop [tabs-list (get-mem :tabs-list)]
    (if (empty? tabs-list)
      nil
      (let [tab-name (:name (first tabs-list))
            tree (get-mem [:tab-tree-map tab-name])
            model (config tree :model)]
        (log-pr 2 'pruning tab-name)
        (delete-last-event-if-too-many model config/max-nodes-per-tab)
        (recur (rest tabs-list))))))


