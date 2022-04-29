(ns more-speech.ui.swing.main-window
  (:require [clojure.core.async :as async]
            [more-speech.nostr.util :as util]
            [more-speech.ui.formatters :as formatters]
            [more-speech.nostr.events :as events]
            [clojure.string :as string]
            [more-speech.ui.config :as config]
            [more-speech.nostr.elliptic-signature :as ecc])
  (:use [seesaw core font tree])
  (:import (javax.swing Timer)))

(declare display-jframe
         action-event
         draw-events
         load-header-tree render-event
         format-article
         prepend>
         send-msg
         make-edit-window)

(defn setup-jframe [event-agent]
  (invoke-later (display-jframe event-agent)))

(defn display-jframe [event-agent]
  (let [main-frame (frame :title "More Speech" :size [1000 :by 1000])
        article-area (text :multi-line? true
                           :font config/default-font
                           :editable? false)
        header-tree (tree :renderer (partial render-event event-agent))
        timer (Timer. 100 nil)
        reply-button (button :text "Reply")
        create-button (button :text "Create")]
    (listen main-frame :window-closing (fn [_]
                                         (.stop timer)
                                         (async/>!! (:send-chan @event-agent)
                                                    [:closed])
                                         (.dispose main-frame)))

    (listen timer :action
            (fn [_] (when (:update @event-agent)
                      (config! header-tree
                               :model (load-header-tree @event-agent))
                      (send event-agent events/updated))))

    (listen header-tree :selection
            (fn [e]
              (when (last (selection e))
                (let [selected-id (last (selection e))
                      event-state @event-agent
                      text-map (:text-event-map event-state)
                      event (get text-map selected-id)]
                  (text! article-area (format-article event-state event))))))

    (listen reply-button :action
            (fn [_]
              (make-edit-window :reply event-agent header-tree)))

    (listen create-button :action
            (fn [_] (make-edit-window :send event-agent nil)))

    (config! main-frame :content (border-panel
                                   :north (scrollable header-tree)
                                   :center (scrollable article-area)
                                   :south (flow-panel :items [reply-button create-button])))
    (show! main-frame)
    (.start timer)))

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
        selected-id (if reply? (last (selection header-tree)) nil)
        event (if reply? (get event-map selected-id) nil)
        ]
    (listen send-button :action
            (fn [_]
              (let [message (text edit-area)]
                (send-msg event-state event message))
              (dispose! edit-frame)))
    (text! edit-area
           (if reply?
             (prepend> (formatters/reformat-article (:content event) 80))
             ""))
    (config! edit-frame :content
             (border-panel
               :center (scrollable edit-area)
               :south (flow-panel :items [send-button])))
    (show! edit-frame)))

(defn format-article [event-state {:keys [id pubkey created-at content]}]
  (let [nicknames (:nicknames event-state)
        time (formatters/format-time created-at)
        name (get nicknames pubkey (util/num->hex-string pubkey))
        name (formatters/abbreviate name 20)
        article (formatters/reformat-article content 80)
        formatted-id (formatters/abbreviate (util/num->hex-string id) 10)]
    (format "%s %20s %s\n%s" time name formatted-id article))
  )

(declare has-children? get-children)

(defn load-header-tree [event-state]
  (simple-tree-model
    (partial has-children? event-state)
    (partial get-children event-state)
    (map first (:chronological-text-events event-state))
    )
  )

(defn has-children? [event-state node]
  (if (seqable? node)
    true
    (let [event-id node
          event (get (:text-event-map event-state) event-id)
          references (:references event)
          ref-count (count references)]
      (pos-int? ref-count))))

(defn get-children [event-state node]
  (if (seqable? node)
    node
    (let [event-id node
          event-map (:text-event-map event-state)
          event (get event-map event-id)
          references (:references event)]
      (sort-by #(:created-at (get event-map %)) references))))

(defn format-event [nicknames {:keys [pubkey created-at content] :as event}]
  (if (nil? event)
    "nil"
    (let [name (get nicknames pubkey (util/num->hex-string pubkey))
          name (formatters/abbreviate name 20)
          time (formatters/format-time created-at)
          content (string/replace content \newline \~)
          content (formatters/abbreviate content 50)]
      (format "%20s %s %s\n" name time content))))

(defn render-event [event-agent widget info]
  (config! widget :font config/default-font)
  (if (seqable? (:value info))
    (text! widget "Articles")
    (let [event-state @event-agent
          nicknames (:nicknames event-state)
          event-map (:text-event-map event-state)
          event-id (:value info)
          event (get event-map event-id)]
      (text! widget (format-event nicknames event)))
    ))

(defn prepend> [text]
  (let [lines (string/split-lines text)
        lines (map #(str ">" %) lines)]
    (string/join "\n" lines)))

(defn send-msg [event-state event message]
  (let [private-key (get-in event-state [:keys :private-key])
        private-key (ecc/hex-string->bytes private-key)
        reply-to (:id event)
        event (events/compose-text-event private-key message reply-to)
        send-chan (:send-chan event-state)]
    (async/>!! send-chan [:event event])))