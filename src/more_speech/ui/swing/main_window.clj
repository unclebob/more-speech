(ns more-speech.ui.swing.main-window
  (:require [clojure.core.async :as async]
            [clojure.java.browse :as browse]
            [more-speech.nostr.events :as events]
            [more-speech.ui.swing.article-tree :as article-tree]
            [more-speech.ui.swing.article-panel :as article-panel]
            [more-speech.ui.swing.relay-panel :as relay-panel]
            [more-speech.ui.swing.util :as util]
            [more-speech.ui.swing.ui-context :refer :all])
  (:use [seesaw core])
  (:import (javax.swing Timer)
           (javax.swing.event HyperlinkEvent$EventType)))

(defrecord seesawHandler []
  events/event-handler
  (handle-text-event [_handler event]
    (invoke-now (article-tree/add-event event)))
  (update-relay-panel [_handler]
    (invoke-later (relay-panel/update-relay-panel ui-context))))

(declare make-main-window)

(defn setup-main-window []
  (invoke-now (make-main-window))
  (->seesawHandler))

(declare timer-action
         get-tabs-with-all
         tab-menu)

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
              tab-data {:title tab-label
                        :content (scrollable header-tree
                                             :id (keyword (str "tab-" (name tab-name))))}]
          (recur (rest tab-names) (conj header-tree-tabs tab-data)))))))

(declare change-tab-name
         delete-tab)

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

(defn open-link [e]
  (if (= HyperlinkEvent$EventType/ACTIVATED (.getEventType e))
         (browse/browse-url (.getURL e))))

(defn make-main-window []
  (let [main-frame (frame :title "More Speech" :size [1500 :by 1000])
        _ (swap! ui-context assoc :frame main-frame)
        article-area (article-panel/make-article-area)
        _ (listen article-area :hyperlink open-link)
        header-tab-panel (tabbed-panel :tabs (make-tabs) :id :header-tab-panel)
        relay-panel (relay-panel/make-relay-panel)
        header-panel (left-right-split (scrollable relay-panel)
                                       header-tab-panel)
        article-panel (border-panel :north (article-panel/make-article-info-panel)
                                    :center (scrollable article-area)
                                    :south (article-panel/make-control-panel))
        main-panel (top-bottom-split
                     header-panel
                     article-panel
                     :divider-location 1/2)
        timer (Timer. 100 nil)]
    (config! main-frame :content main-panel)
    (listen timer :action timer-action)
    (listen main-frame :window-closing
            (fn [_]
              (.stop timer)
              (let [event-context (:event-context @ui-context)
                    send-chan (:send-chan @event-context)]
                (async/>!! send-chan [:closed]))
              (.dispose main-frame)))
    (show! main-frame)
    (.start timer)))

(defn timer-action [_]
  ;nothing for now.
  )


