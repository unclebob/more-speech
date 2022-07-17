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
  (let [event-context (:event-context @ui-context)]
    (loop [tabs-list (:tabs-list @event-context)
           header-tree-tabs []
           tab-index 0]
      (if (empty? tabs-list)
        header-tree-tabs
        (let [tab (first tabs-list)
              tab-name (:name tab)
              header-tree (article-tree/make-header-tree tab-index)
              _ (config! header-tree
                         :user-data tab-index
                         :id (keyword (str tab-index)))
              tab-label (label :text tab-name)
              _ (listen tab-label :mouse-pressed tab-menu)
              tab-content (scrollable header-tree)
              tab-data {:title tab-label
                        :content tab-content}]
          (recur (rest tabs-list)
                 (conj header-tree-tabs tab-data)
                 (inc tab-index)))))))

(defn tab-menu [e]
  (let [tab-label (.getComponent e)
        tab-name (config tab-label :text)
        isAll? (= "all" tab-name)
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

(defn ensure-tab-list-has-all [tab-list]
  (if (some #(= "all" (:name %)) tab-list)
    tab-list
    (conj tab-list {:name "all" :selected [] :blocked []})))

