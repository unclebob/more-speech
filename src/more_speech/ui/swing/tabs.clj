(ns more-speech.ui.swing.tabs
  (:require [more-speech.ui.swing.util :as util]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.ui.swing.article-tree :as article-tree])
  (:use [seesaw core]))


(declare change-tab-name
         delete-tab
         tab-menu
         get-tabs-with-all)

(defn make-tabs []
  (let [event-context (:event-context @ui-context)
        tabs (get-tabs-with-all event-context)]
    (loop [tab-names (keys tabs)
           header-tree-tabs []]
      (if (empty? tab-names)
        header-tree-tabs
        (let [tab-name (first tab-names)
              header-tree (article-tree/make-header-tree tab-name)
              _ (config! header-tree
                         :user-data (get tabs tab-name)
                         :id tab-name)
              tab-label (label :text (name tab-name)
                               :user-data tab-name)
              _ (listen tab-label :mouse-pressed tab-menu)
              tab-content (scrollable header-tree
                                :id (keyword (str "tab-" (name tab-name))))
              tab-data {:title tab-label
                        :content tab-content}]
          (recur (rest tab-names) (conj header-tree-tabs tab-data)))))))

(defn tab-menu [e]
  (let [tab-label (.getComponent e)
        tab-name (config tab-label :user-data)
        isAll? (= :all tab-name)
        p (popup :items [(action :name "Change name..."
                                 :handler (partial change-tab-name tab-name)
                                 :enabled? (not isAll?))
                         (action :name "Delete"
                                 :handler (partial delete-tab tab-name)
                                 :enabled? (not isAll?))])]
    (if (.isPopupTrigger e)
      (.show p (to-widget e) (.x (.getPoint e)) (.y (.getPoint e)))
      (util/select-tab tab-name))
    ))

(defn change-tab-name [tab-name _e]
  (prn 'change-tab-name tab-name))

(defn delete-tab [tab-name _e]
  (prn 'delete-tab tab-name))

(defn get-tabs-with-all [event-context]
  (let [tabs (:tabs @event-context)]
    (when-not (contains? tabs :all)
      (swap! event-context assoc-in [:tabs :all] {:selected [] :blocked []}))
    (:tabs @event-context)))

(defn ensure-tab-list-has-all [tab-list]
  (if (some #(= "all" (:name %)) tab-list)
    tab-list
    (conj tab-list {:name "all" :selected [] :blocked []})))