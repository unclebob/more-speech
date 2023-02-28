(ns more-speech.ui.swing.main-window
  (:require [clojure.core.async :as async]
            [clojure.java.browse :as browse]
            [more-speech.db.gateway :as gateway]
            [more-speech.nostr.event-handlers :as handlers]
            [more-speech.ui.swing.article-tree :as article-tree]
            [more-speech.ui.swing.article-panel :as article-panel]
            [more-speech.ui.swing.tabs :as tabs]
            [more-speech.ui.swing.util :as swing-util]
            [more-speech.ui.swing.relay-manager :as relay-manager]
            [more-speech.mem :refer :all]
            [more-speech.config :as config :refer [get-db]]
            [more-speech.ui.formatter-util :as formatter-util]
            [more-speech.nostr.util :as util]
            [clojure.string :as string])
  (:use [seesaw core])
  (:import (java.util Timer TimerTask)
           (javax.swing.event HyperlinkEvent$EventType)))

(defrecord seesawHandler []
  handlers/event-handler
  (handle-text-event [_handler event]
    (invoke-later (article-tree/add-event event)))
  (immediate-add-text-event [_handler event]
    (article-tree/add-event event)))

(defn open-link [e]
  (when (= HyperlinkEvent$EventType/ACTIVATED (.getEventType e))
    (when-let [url (str (.getURL e))]
      (let [[type subject] (string/split (.getDescription e) #"://")]
        (cond
          (or (= type "http") (= type "https"))
          (try
            (browse/browse-url url)
            (catch Exception ex
              (prn 'open-link url (.getMessage ex))
              (prn ex)))

          (= type "ms-idreference")
          (let [id (util/unhexify (subs subject 1))]
            (swing-util/select-event id))

          :else
          (do (prn 'open-link url 'type type 'subject subject)
              (prn (.getDescription e)))
          )))))

(defn make-profile-line [id]
  (let [profile (gateway/get-profile (get-db) id)
        name (formatter-util/abbreviate (:name profile) 20)]
    (format "%-20s %s %s" name (util/num32->hex-string id) (:picture profile))))

(defn show-kinds [stats-panel]
  (doseq [kind (keys (get-mem [:event-counter :kinds]))]
    (let [id (keyword (str "#kind-" kind))]
      (config! (select stats-panel [id])
               :text (str (get-mem [:event-counter :kinds kind]))))))

(defn show-status [stats-panel]
  (config! (select stats-panel [:#backlog-data])
           :text (str (get-mem :websocket-backlog)))
  (config! (select stats-panel [:#processed-data])
           :text (str (get-mem [:event-counter :total])))
  (config! (select stats-panel [:#incoming-data])
           :text (str (get-mem [:incoming-events])))
  (config! (select stats-panel [:#dups-data])
           :text (str (get-mem [:event-counter :dups])))
  (show-kinds stats-panel))

(defn close-stats-frame [timer menu _e]
  (config! menu :enabled? true)
  (.cancel timer))

(defn make-stat-panel [name id]
  (let [stat-label (label name)
        stat-data (label :text "" :id id :size [100 :by 20])
        stat-panel (left-right-split stat-data stat-label)]
    stat-panel))

(defn make-kind-panels []
  (loop [kinds (sort (keys (get-mem [:event-counter :kinds])))
         kind-panels []]
    (if (empty? kinds)
      kind-panels
      (let [kind (first kinds)
            kind-panel (make-stat-panel (str "Kind:" kind)
                                        (keyword (str "kind-" kind)))]
        (recur (rest kinds) (conj kind-panels kind-panel)))))
  )

(defn make-stats-frame [_e]
  (let [stats-frame (frame :title "Stats")
        incoming-panel (make-stat-panel "Incoming events." :incoming-data)
        backlog-panel (make-stat-panel "Backlog." :backlog-data)
        processed-panel (make-stat-panel "Processed events." :processed-data)
        dups-panel (make-stat-panel "Duplicate events." :dups-data)
        kind-panels (make-kind-panels)

        stats-panel (vertical-panel :items (concat [incoming-panel
                                                    processed-panel
                                                    backlog-panel
                                                    dups-panel]
                                                   kind-panels))

        stats-timer (Timer. "stats timer")
        show-status-task (proxy [TimerTask] []
                           (run [] (show-status stats-panel)))

        stats-menu (select (get-mem :frame) [:#stats-menu])]
    (config! stats-frame :content stats-panel)
    (config! stats-menu :enabled? false)
    (listen stats-frame :window-closing (partial close-stats-frame stats-timer stats-menu))
    (pack! stats-frame)
    (show! stats-frame)
    (.schedule stats-timer show-status-task 1000 1000)))

(defn make-menubar []
  (let [relays-item (menu-item :action (action :name "Relays..." :handler relay-manager/show-relay-manager)
                               :id :relays-menu)
        stats-item (menu-item :action (action :name "Stats..." :handler make-stats-frame)
                              :id :stats-menu)
        users-item (menu-item :text "Users...")
        profile-item (menu-item :text "Profile...")
        manage-menu (menu :text "Manage" :items [relays-item stats-item users-item profile-item])
        menu-bar (menubar :items [manage-menu])]
    menu-bar))

(defn make-main-window []
  (prn 'make-main-window)
  (let [title (str "More-Speech:" (:name (get-mem :keys)) " - " config/version)
        title (if (config/is-test-run?) (str title " - TEST") title)
        main-frame (frame :title title :size [1000 :by 1000] :menubar (make-menubar))
        _ (set-mem :frame main-frame)
        _ (prn 'make-main-window 'making-article-area)
        article-area (article-panel/make-article-area)
        _ (listen article-area :hyperlink open-link)
        header-tab-panel (tabbed-panel :tabs (tabs/make-tabs) :id :header-tab-panel)
        article-panel (border-panel :north (article-panel/make-article-info-panel)
                                    :center (scrollable article-area)
                                    :south (article-panel/make-control-panel))
        _ (prn 'make-main-window 'article-panel-complete)
        messages-panel (top-bottom-split
                         header-tab-panel
                         article-panel
                         :divider-location 1/2)
        _ (prn 'make-main-window 'messages-panel-complete)]
    (config! main-frame :content messages-panel)
    (listen main-frame :window-closing
            (fn [_]
              (let [send-chan (get-mem :send-chan)]
                (future (async/>!! send-chan [:closed])))
              (.dispose main-frame)))
    (prn 'make-main-window 'showing-main-frame)
    (show! main-frame)
    (prn 'make-main-window 'shown)))

(defn setup-main-window []
  (invoke-now (make-main-window))
  (prn 'setup-main-window 'creating-seesaw-handler)
  (->seesawHandler))





