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
              tab-label (label :text tab-name :user-data tab-index)
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
        tab-index (config tab-label :user-data)
        isAll? (= "all" tab-name)
        p (popup :items [(action :name "Change name..."
                                 :handler (partial change-tab-name tab-label)
                                 :enabled? (not isAll?))
                         (action :name "Delete"
                                 :handler (partial delete-tab tab-label)
                                 :enabled? (not isAll?))])]
    (if (.isPopupTrigger e)
      (.show p (to-widget e) (.x (.getPoint e)) (.y (.getPoint e)))
      (util/select-tab tab-index))
    ))

(defn change-tab-name [tab-label _e]
  (let [tab-name (config tab-label :text)
        new-name (input (format "rename %s to:" tab-name))
        new-name (if (some? (util/get-tab-index new-name))
                   (str new-name "-" (rand-int 1000000))
                   new-name)

        frame (:frame @ui-context)
        tab-panel (select frame [:#header-tab-panel])
        tab-index (config tab-label :user-data)]
    (if (some? tab-index)
      (let [label (.getTabComponentAt tab-panel tab-index)]
        (config! label :text new-name)
        (util/change-tabs-list-name tab-name new-name))
      (prn 'bad-tab-name tab-name))
    ))

(defn delete-tab [tab-label _e]
  (let [tab-name (config tab-label :text)]
    (prn 'delete-tab tab-name)
    (prn (confirm (format "NOT IMPLEMENTED: Confirm delete %s" tab-name)))))

(defn ensure-tab-list-has-all [tab-list]
  (if (some #(= "all" (:name %)) tab-list)
    tab-list
    (conj tab-list {:name "all" :selected [] :blocked []})))

