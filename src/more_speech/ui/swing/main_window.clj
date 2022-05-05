(ns more-speech.ui.swing.main-window
  (:require [clojure.core.async :as async]
            [more-speech.ui.formatters :as formatters]
            [more-speech.nostr.events :as events]
            [more-speech.ui.config :as config]
            [more-speech.ui.swing.article-tree :as article-tree])
  (:use [seesaw core font tree])
  (:import (javax.swing Timer)))

(def ui-context (atom {:frame nil
                       :event-agent nil
                       :node-map {}}))

(defrecord seesawHandler []
  events/event-handler
  (events/handle-text-event [_handler event]
    (invoke-later (article-tree/add-event ui-context event))))

(declare make-main-window)

(defn setup-main-window [event-agent]
  (invoke-now (make-main-window event-agent))
  (->seesawHandler))

(declare make-edit-window
         make-article-info-panel
         make-article-area
         make-control-panel
         timer-action)

(defn make-main-window [event-agent]
  (let [main-frame (frame :title "More Speech" :size [1000 :by 1000])
        article-area (make-article-area)
        header-tree (article-tree/make-article-tree event-agent main-frame)
        article-panel (border-panel :north (make-article-info-panel)
                                    :center (scrollable article-area)
                                    :south (make-control-panel event-agent header-tree))
        main-panel (top-bottom-split
                     (scrollable header-tree)
                     article-panel)
        timer (Timer. 100 nil)]
    (config! main-frame :content main-panel)
    (swap! ui-context assoc :frame main-frame :event-agent event-agent)

    (listen timer :action timer-action)

    (listen main-frame :window-closing
            (fn [_]
              (.stop timer)
              (async/>!! (:send-chan @event-agent)
                         [:closed])
              (.dispose main-frame)))

    (show! main-frame)
    (.start timer)))

(defn make-article-info-panel []
  (grid-panel
    :rows 2 :columns 2
    :items [(label :id :author-id)
            (label :id :created-at)
            (label :id :reply-to)
            (label :id :citing)]))

(defn make-article-area []
  (text :multi-line? true
        :font config/default-font
        :editable? false
        :id :article-area))

(defn make-control-panel [event-agent header-tree]
  (let [reply-button (button :text "Reply")
        create-button (button :text "Create")]
    (listen reply-button :action
            (fn [_]
              (make-edit-window :reply event-agent header-tree)))

    (listen create-button :action
            (fn [_] (make-edit-window :send event-agent nil)))
    (flow-panel :items [reply-button create-button])))

(defn timer-action [_]
  ;nothing for now.
  )



(defn make-edit-window [kind event-agent header-tree]
  (let [reply? (= kind :reply)
        event-state @event-agent
        edit-frame (frame :title (name kind)
                          :size [1000 :by 500]
                          :on-close :dispose)
        edit-area (text :multi-line? true
                        :font config/default-font)
        send-button (button :text "Send")
        event-map (:text-event-map event-state)
        selected-id (if reply?
                      (.getUserObject (last (selection header-tree)))
                      nil)
        event (if reply?
                (get event-map selected-id)
                nil)]
    (listen send-button :action
            (fn [_]
              (let [message (text edit-area)]
                (events/send-msg event-state event message))
              (dispose! edit-frame)))
    (text! edit-area
           (if reply?
             (formatters/format-reply event)
             ""))
    (config! edit-frame :content
             (border-panel
               :center (scrollable edit-area)
               :south (flow-panel :items [send-button])))
    (show! edit-frame)))
