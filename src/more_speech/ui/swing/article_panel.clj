(ns more-speech.ui.swing.article-panel
  (:require [more-speech.ui.swing.edit-window :as edit-window]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.ui.swing.article-tree-util :as article-tree-util]
            [more-speech.ui.config :as config]
            [more-speech.ui.formatters :as formatters]
            [more-speech.nostr.events :as events]
            [more-speech.nostr.util :as util])
  (:use [seesaw core])
  )

(declare id-click)

(defn make-article-info-panel []
  (let [author-id-label (label :id :author-id-label)
        created-time-label (label :id :created-time-label)
        reply-to-label (label :id :reply-to-label)
        id-label (label :id :id-label)
        citing-label (label :id :citing-label)
        root-label (label :id :root-label)
        relays-label (label :id :relays-label)]
    (listen citing-label :mouse-pressed id-click)
    (listen root-label :mouse-pressed id-click)
    (grid-panel
      :rows 3 :columns 3
      :items [(flow-panel :align :left :items [(label "author:") author-id-label])
              (flow-panel :align :left :items [(label "created at:") created-time-label])
              (flow-panel :align :left :items [(label "reply to:") reply-to-label])
              (flow-panel :align :left :items [(label "id:") id-label])
              (flow-panel :align :left :items [(label "citing:") citing-label])
              (flow-panel :align :left :items [(label "root:") root-label])
              (flow-panel :align :left :items [(label "relays:") relays-label])
              ])))

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
              (edit-window/make-edit-window :reply event-agent header-tree)))

    (listen create-button :action
            (fn [_] (edit-window/make-edit-window :send event-agent nil)))
    (flow-panel :items [reply-button create-button])))

(defn id-click [e]
  (article-tree-util/id-click ui-context (config e :user-data)))

(defn load-article-info [event-state selected-id main-frame]
  (let [nicknames (:nicknames event-state)
        format-user (partial formatters/format-user-id nicknames)
        text-map (:text-event-map event-state)
        event (get text-map selected-id)
        [root-id _ referent] (events/get-references event)
        reply-to (select main-frame [:#reply-to-label])
        citing (select main-frame [:#citing-label])
        root-label (select main-frame [:#root-label])
        relays-label (select main-frame [:#relays-label])
        article-area (select main-frame [:#article-area])]
    (text! article-area (formatters/reformat-article (:content event) 80))
    (text! (select main-frame [:#author-id-label])
           (format-user (:pubkey event)))
    (text! (select main-frame [:#created-time-label])
           (formatters/format-time (:created-at event)))
    (config! (select main-frame [:#id-label])
             :user-data (:id event)
             :text (formatters/abbreviate (util/num32->hex-string (:id event)) 30))
    (if (some? referent)
      (let [replied-event (get text-map referent)]
        (text! reply-to (format-user (:pubkey replied-event)))
        (config! citing
                 :user-data referent
                 :text (formatters/abbreviate (util/num32->hex-string referent) 30)))
      (do (text! reply-to "")
          (text! citing "")))
    (if (some? root-id)
      (config! root-label
               :user-data root-id
               :text (formatters/abbreviate (util/num32->hex-string root-id) 30))
      (text! root-label ""))
    (text! relays-label (pr-str (count (:relays event)) (first (:relays event))))
    ))