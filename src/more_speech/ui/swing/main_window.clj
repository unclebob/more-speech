(ns more-speech.ui.swing.main-window
  (:require [clojure.core.async :as async]
            [more-speech.logger.default :refer [log-pr]]
            [more-speech.db.gateway :as gateway]
            [more-speech.nostr.event-handlers :as handlers]
            [more-speech.ui.swing.article-tree :as article-tree]
            [more-speech.ui.swing.article-panel :as article-panel]
            [more-speech.ui.swing.tabs :as tabs]
            [more-speech.ui.swing.relay-manager :as relay-manager]
            [more-speech.ui.swing.stats-window :as stats-window]
            [more-speech.ui.swing.profile-window :as profile-window]
            [more-speech.mem :refer :all]
            [more-speech.config :as config :refer [get-db]]
            [more-speech.ui.formatter-util :as formatter-util]
            [more-speech.nostr.util :as util])
  (:use [seesaw core])
  (:import (java.util Timer TimerTask)))

(defrecord seesawHandler []
  handlers/event-handler
  (handle-text-event [_handler event]
    (invoke-later (article-tree/add-event event)))
  (immediate-add-text-event [_handler event]
    (more-speech.logger.default/set-level 3)
    (article-tree/add-event event)
    (more-speech.logger.default/set-level 1)))

(defn make-profile-line [id]
  (let [profile (gateway/get-profile (get-db) id)
        name (formatter-util/abbreviate (:name profile) 20)]
    (format "%-20s %s %s" name (util/num32->hex-string id) (:picture profile))))

(defn make-menubar []
  (let [relays-item (menu-item :action (action :name "Relays..." :handler relay-manager/show-relay-manager)
                               :id :relays-menu)
        stats-item (menu-item :action (action :name "Stats..."
                                              :handler stats-window/make-stats-frame)
                              :id :stats-menu)
        users-item (menu-item :text "Users...")
        profile-item (menu-item :action (action :name "Profile..."
                                                :handler profile-window/make-profile-frame)
                                :id :profile-menu)
        manage-menu (menu :text "Manage" :items [relays-item stats-item users-item profile-item])
        menu-bar (menubar :items [manage-menu])]
    menu-bar))

(defn make-main-window []
  (log-pr 1 'make-main-window)
  (let [title (str "More-Speech:" (:name (get-mem :keys)) " - " config/version)
        title (if (config/is-test-run?) (str title " - TEST") title)
        main-frame (frame :title title :size [1000 :by 1000] :menubar (make-menubar))
        _ (set-mem :frame main-frame)
        _ (log-pr 1 'make-main-window 'making-article-area)
        article-area (article-panel/make-article-area)
        _ (listen article-area :hyperlink article-panel/open-link)
        header-tab-panel (tabbed-panel :tabs (tabs/make-tabs) :id :header-tab-panel)
        article-panel (border-panel :north (article-panel/make-article-info-panel)
                                    :center (scrollable article-area)
                                    :south (article-panel/make-control-panel))
        _ (log-pr 1 'make-main-window 'article-panel-complete)
        messages-panel (top-bottom-split
                         header-tab-panel
                         article-panel
                         :divider-location 1/2)
        _ (log-pr 1 'make-main-window 'messages-panel-complete)]
    (config! main-frame :content messages-panel)
    (listen main-frame :window-closing
            (fn [_]
              (let [send-chan (get-mem :send-chan)]
                (future (async/>!! send-chan [:closed])))
              (.dispose main-frame)))
    (log-pr 2 'make-main-window 'showing-main-frame)
    (show! main-frame)
    (log-pr 2 'make-main-window 'shown)))

(defn setup-main-timer []
  (let [main-timer (Timer. "main timer")
        prune-tabs-task (proxy [TimerTask][]
                         (run [] (tabs/prune-tabs)))
        prune-tabs-frequency (* config/prune-tabs-frequency-in-minutes 60 1000)]
    (.schedule main-timer
               prune-tabs-task
               (long prune-tabs-frequency)
               (long prune-tabs-frequency))))

(defn setup-main-window []
  (setup-main-timer)
  (invoke-now (make-main-window))
  (log-pr 2 'setup-main-window 'creating-seesaw-handler)
  (->seesawHandler))





