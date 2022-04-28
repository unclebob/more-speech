(ns more-speech.ui.swing.main-window
  (:require [clojure.core.async :as async]
            [more-speech.nostr.util :as util]
            [more-speech.ui.formatters :as formatters]
            [more-speech.nostr.events :as events]
            [clojure.string :as string])
  (:use [seesaw core font tree])
  (:import (javax.swing SwingUtilities Timer)))

(declare display-jframe action-event draw-events load-header-tree render-event)

(defn setup-jframe [event-agent output-channel]
  (SwingUtilities/invokeLater #(display-jframe event-agent output-channel)))

(defn display-jframe [event-agent output-channel]
  (let [frame (frame :title "More Speech" :size [1000 :by 1000])
        header-tree (tree :renderer (partial render-event event-agent))
        timer (Timer. 100 nil)]
    (listen frame :window-closing (fn [_]
                                    (.stop timer)
                                    (async/>!! output-channel [:closed])
                                    (.dispose frame)))

    (listen timer :action
            (fn [_] (when (:update @event-agent)
                      (config! header-tree
                               :model (load-header-tree @event-agent))
                      (send event-agent events/updated))))
    (config! frame :content (scrollable header-tree))
    (show! frame)
    (.start timer)))

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
  (config! widget :font "COURIER-PLAIN-14")
  (if (seqable? (:value info))
    (text! widget "Articles")
    (let [event-state @event-agent
          nicknames (:nicknames event-state)
          event-map (:text-event-map event-state)
          event-id (:value info)
          event (get event-map event-id)]
      (text! widget (format-event nicknames event)))
    ))