(ns more-speech.ui.swing.main-window
  (:require [clojure.core.async :as async]
            [clojure.java.browse :as browse]
            [more-speech.db.gateway :as gateway]
            [more-speech.nostr.event-handlers :as handlers]
            [more-speech.ui.swing.article-tree :as article-tree]
            [more-speech.ui.swing.article-panel :as article-panel]
            [more-speech.ui.swing.relay-panel :as relay-panel]
            [more-speech.ui.swing.tabs :as tabs]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.config :as config :refer [get-db]]
            [more-speech.ui.formatter-util :as formatter-util]
            [more-speech.nostr.util :as util])
  (:use [seesaw core])
  (:import (javax.swing Timer)
           (javax.swing.event HyperlinkEvent$EventType)))

(defrecord seesawHandler []
  handlers/event-handler
  (handle-text-event [_handler event]
    (invoke-now (article-tree/add-event event)))
  (update-relay-panel [_handler]
    (invoke-later (relay-panel/update-relay-panel))))

(defn open-link [e]
  (when (= HyperlinkEvent$EventType/ACTIVATED (.getEventType e))
    (when-let [url (str (.getURL e))]
      (try
        (browse/browse-url url)
        (catch Exception ex
          (prn 'open-link url (.getMessage ex))
          (prn ex))))))

(defn timer-action [_]
  ;nothing for now.
  )

(defn make-profile-line [id]
  (let [profile (gateway/get-profile (get-db) id)
        name (formatter-util/abbreviate (:name profile) 20)]
    (format "%-20s %s %s" name (util/num32->hex-string id) (:picture profile))))

(defn make-main-window []
  (prn 'make-main-window)
  (let [title (str "More-Speech:" (:name (get-mem :keys)) " - " config/version)
        main-frame (frame :title title :size [1500 :by 1000])
        _ (set-mem :frame main-frame)
        _ (prn 'make-main-window 'making-article-area)
        article-area (article-panel/make-article-area)
        _ (listen article-area :hyperlink open-link)
        header-tab-panel (tabbed-panel :tabs (tabs/make-tabs) :id :header-tab-panel)
        _ (prn 'make-main-window 'making-relay-panel)
        relay-panel (relay-panel/make-relay-panel)
        _ (prn 'make-main-window 'relay-panel-complete)
        article-panel (border-panel :north (article-panel/make-article-info-panel)
                                    :center (scrollable article-area)
                                    :south (article-panel/make-control-panel))
        _ (prn 'make-main-window 'article-panel-complete)
        messages-panel (top-bottom-split
                         header-tab-panel
                         article-panel
                         :divider-location 1/2)
        _ (prn 'make-main-window 'messages-panel-complete)
        main-tabs (tabbed-panel :tabs [{:title "Messages" :content messages-panel}
                                       {:title "Relays" :content (scrollable relay-panel)}])
        timer (Timer. 100 nil)]
    (config! main-frame :content main-tabs)
    (listen timer :action timer-action)
    (listen main-frame :window-closing
            (fn [_]
              (.stop timer)
              (let [send-chan (get-mem :send-chan)]
                (future (async/>!! send-chan [:closed])))
              (.dispose main-frame)))
    (prn 'make-main-window 'showing-main-frame)
    (show! main-frame)
    (prn 'make-main-window 'shown)
    (.start timer)))

(defn setup-main-window []
  (invoke-now (make-main-window))
  (prn 'setup-main-window 'creating-seesaw-handler)
  (->seesawHandler))





