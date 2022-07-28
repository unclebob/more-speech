(ns more-speech.ui.swing.main-window
  (:require [clojure.core.async :as async]
            [clojure.java.browse :as browse]
            [more-speech.nostr.events :as events]
            [more-speech.ui.swing.article-tree :as article-tree]
            [more-speech.ui.swing.article-panel :as article-panel]
            [more-speech.ui.swing.relay-panel :as relay-panel]
            [more-speech.ui.swing.tabs :as tabs]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.config :as config])
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

(declare timer-action)

(defn open-link [e]
  (when (= HyperlinkEvent$EventType/ACTIVATED (.getEventType e))
         (browse/browse-url (.getURL e))))

(defn make-main-window []
  (let [event-context @(:event-context @ui-context)
        title (str "More-Speech:" (:name (:keys event-context)) " - " config/version)
        main-frame (frame :title title :size [1500 :by 1000])
        _ (swap! ui-context assoc :frame main-frame)
        article-area (article-panel/make-article-area)
        _ (listen article-area :hyperlink open-link)
        header-tab-panel (tabbed-panel :tabs (tabs/make-tabs) :id :header-tab-panel)
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
                (future (async/>!! send-chan [:closed])))
              (.dispose main-frame)))
    (show! main-frame)
    (.start timer)))

(defn timer-action [_]
  ;nothing for now.
  )


