(ns more-speech.ui.swing.edit-window
  (:require [more-speech.nostr.events :as events]
            [more-speech.ui.config :as config]
            [more-speech.ui.formatters :as formatters]
            [more-speech.ui.swing.ui-context :refer :all])
  (:use [seesaw core]))

(defn make-edit-window [kind]
  (let [reply? (= kind :reply)
        event-agent (:event-agent @ui-context)
        event-state @event-agent
        subject-label (label "Subject:")
        subject-text (text :id :subject :text "")
        subject-panel (left-right-split subject-label subject-text
                                        :divider-location 1/10)
        edit-frame (frame :title (name kind)
                          :size [1000 :by 500]
                          :on-close :dispose)
        edit-area (editor-pane :font config/default-font)
        send-button (button :text "Send")
        event-map (:text-event-map event-state)
        selected-id (if reply? (:selected-event @event-agent) nil)
        event (if reply? (get event-map selected-id) nil)]
    (when reply?
      (let [subject (formatters/get-subject (:tags event))
            prefix (if (empty? subject) "" "Re: ")]
        (text! subject-text (str prefix subject))))
    (listen send-button :action
            (fn [_]
              (let [message (text edit-area)
                    subject (text subject-text)]
                (events/compose-and-send-text-event event-state event subject message))
              (dispose! edit-frame)))
    (text! edit-area
           (if reply?
             (formatters/format-reply event)
             ""))
    (config! edit-frame :content
             (border-panel
               :north subject-panel
               :center (scrollable edit-area)
               :south (flow-panel :items [send-button])))
    (show! edit-frame)))