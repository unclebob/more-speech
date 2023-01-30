(ns more-speech.ui.swing.edit-window
  (:require [more-speech.nostr.event-composers :as composers]
            [more-speech.ui.formatters :as formatters]
            [more-speech.mem :refer :all]
            [more-speech.user-configuration :as uconfig]
            [more-speech.config :refer [get-db]]
            [more-speech.db.gateway :as gateway])
  (:use [seesaw core]))

(defn make-edit-window
  ([kind]
   (make-edit-window kind ""))

  ([kind content]
   (let [reply? (= kind :reply)
         subject-label (label "Subject:")
         subject-text (text :id :subject :text "")
         subject-panel (left-right-split subject-label subject-text
                                         :divider-location 1/10)
         edit-frame (frame :title (name kind)
                           :size [1000 :by 500]
                           :on-close :dispose)
         edit-area (styled-text :font (uconfig/get-default-font)
                                :wrap-lines? true)
         send-button (button :text "Send")
         selected-id (if reply? (get-mem :selected-event) nil)
         db (get-db)
         event (if reply?
                 (gateway/get-event db selected-id)
                 nil)]
     (when (or (not reply?)
               (and reply? (some? event)))
       (when reply?
         (let [subject (formatters/get-subject (:tags event))
               prefix (if (empty? subject) "" "Re: ")]
           (text! subject-text (str prefix subject))))
       (listen send-button :action
               (fn [_]
                 (let [message (text edit-area)
                       subject (text subject-text)]
                   (composers/compose-and-send-text-event event subject message))
                 (dispose! edit-frame)))
       (text! edit-area
              (if reply?
                (formatters/format-reply event)
                content))
       (config! edit-frame :content
                (border-panel
                  :north subject-panel
                  :center (scrollable edit-area)
                  :south (flow-panel :items [send-button])))
       (show! edit-frame)))))
