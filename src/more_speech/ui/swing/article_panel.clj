(ns more-speech.ui.swing.article-panel
  (:require [more-speech.ui.swing.edit-window :as edit-window]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.ui.swing.article-tree-util :as article-tree-util]
            [more-speech.ui.config :as config]
            [more-speech.ui.formatters :as formatters]
            [more-speech.nostr.events :as events]
            [more-speech.nostr.util :as util]
            [more-speech.ui.swing.util :as swing-util])
  (:use [seesaw core])
  )

(declare id-click)

(defn make-article-info-panel []
  (let [author-name-label (label :id :author-name-label)
        author-id-label (text :id :author-id-label :editable? false :font config/small-font)
        created-time-label (label :id :created-time-label)
        reply-to-label (label :id :reply-to-label)
        id-label (text :id :id-label :editable? false :font config/small-font)
        citing-label (text :id :citing-label :editable? false :font config/small-font)
        subject-label (label :id :subject-label)
        root-label (text :id :root-label :editable? false :font config/small-font)
        relays-popup (popup :enabled? false)
        relays-label (label :id :relays-label :user-data relays-popup)]
    (listen relays-label
            :mouse-entered (fn [e]
                             (-> relays-popup
                                 (move! :to (.getLocationOnScreen e))
                                 show!))
            :mouse-exited (fn [_e] (hide! relays-popup)))
    (listen citing-label :mouse-pressed id-click)
    (listen root-label :mouse-pressed id-click)
    (grid-panel
      :rows 3 :columns 3
      :items [(flow-panel :align :left :items [(label "author:") author-name-label])
              (flow-panel :align :left :items [(label "Subject:") subject-label])
              (flow-panel :align :left :items [(label "pubkey:") author-id-label])
              (flow-panel :align :left :items [(label "created at:") created-time-label])
              (flow-panel :align :left :items [(label "reply to:") reply-to-label])
              (flow-panel :align :left :items [(label "relays:") relays-label])
              (flow-panel :align :left :items [(label "id:") id-label])
              (flow-panel :align :left :items [(label "citing:") citing-label])
              (flow-panel :align :left :items [(label "root:") root-label])
              ])))

(defn make-article-area []
  (text :multi-line? true
        :font config/default-font
        :editable? false
        :id :article-area))

(defn make-control-panel []
  (let [reply-button (button :text "Reply")
        create-button (button :text "Create")]
    (listen reply-button :action
            (fn [_]
              (edit-window/make-edit-window :reply)))

    (listen create-button :action
            (fn [_] (edit-window/make-edit-window :send)))
    (flow-panel :items [reply-button create-button])))

(defn id-click [e]
  (article-tree-util/id-click ui-context (config e :user-data)))

(defn load-article-info [selected-id]
  (let [event-state @(:event-agent @ui-context)
        main-frame (:frame @ui-context)
        nicknames (:nicknames event-state)
        format-user (partial formatters/format-user-id nicknames)
        text-map (:text-event-map event-state)
        event (get text-map selected-id)
        [root-id _ referent] (events/get-references event)
        reply-to (select main-frame [:#reply-to-label])
        citing (select main-frame [:#citing-label])
        root-label (select main-frame [:#root-label])
        relays-label (select main-frame [:#relays-label])
        relays-popup (config relays-label :user-data)
        article-area (select main-frame [:#article-area])
        subject-label (select main-frame [:#subject-label])]
    (swing-util/clear-popup relays-popup)
    (config! relays-popup :items (:relays event))
    (text! article-area (formatters/reformat-article (:content event) 80))
    (text! (select main-frame [:#author-name-label])
           (format-user (:pubkey event)))
    (text! (select main-frame [:#author-id-label])
           (util/num32->hex-string (:pubkey event)))
    (text! (select main-frame [:#created-time-label])
           (formatters/format-time (:created-at event)))
    (config! (select main-frame [:#id-label])
             :user-data (:id event)
             :text (util/num32->hex-string (:id event)))
    (if (some? referent)
      (let [replied-event (get text-map referent)]
        (text! reply-to (format-user (:pubkey replied-event)))
        (config! citing
                 :user-data referent
                 :text (util/num32->hex-string referent)))
      (do (text! reply-to "")
          (text! citing "")))
    (if (some? root-id)
      (config! root-label
               :user-data root-id
               :text (util/num32->hex-string root-id))
      (text! root-label ""))
    (text! subject-label (formatters/get-subject (:tags event)))
    (text! relays-label (pr-str (count (:relays event)) (first (:relays event))))
    ))