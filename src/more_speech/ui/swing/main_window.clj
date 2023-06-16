(ns more-speech.ui.swing.main-window
  (:require [clojure.core.async :as async]
            [more-speech.config :as config]                 ;break cycle.
            [more-speech.logger.default :refer [log-pr]]
            [more-speech.mem :refer :all :as mem]
            [more-speech.nostr.event-dispatcher :as handlers]
            [more-speech.ui.swing.article-panel :as article-panel]
            [more-speech.ui.swing.profile-window :as profile-window]
            [more-speech.ui.swing.relay-manager :as relay-manager]
            [more-speech.ui.swing.show-user-info]
            [more-speech.ui.swing.stats-window :as stats-window]
            [more-speech.ui.swing.tabs :as tabs]
            [more-speech.ui.swing.tabs-window :as tabs-window]
            [more-speech.ui.swing.users-window :as users-window])
  (:use (seesaw [core]))
  (:import (java.util Timer TimerTask)))

(defn add-event [event]
  (when (empty? (get-mem [:node-map (:id event)]))
    (loop [tabs (get-mem :tabs-list)]
      (if (empty? tabs)
        nil
        (do
          (tabs/add-event-to-tab (first tabs) event)
          (recur (rest tabs)))))))

(defrecord seesawHandler []
  handlers/event-handler
  (handle-text-event [_handler event]
    (invoke-later (add-event event)))
  (immediate-add-text-event [_handler event]
    (add-event event)))

(defn make-menubar []
  (let [relays-item
        (menu-item :action (action
                             :name "Relays..."
                             :handler relay-manager/show-relay-manager)
                   :id :relays-menu)
        stats-item
        (menu-item :action (action
                             :name "Stats..."
                             :handler stats-window/make-stats-frame)
                   :id :stats-menu)
        users-item
        (menu-item :action (action
                             :name "Users..."
                             :handler users-window/make-users-frame)
                   :id :users-menu)
        profile-item
        (menu-item :action (action
                             :name "Profile..."
                             :handler profile-window/make-profile-frame)
                   :id :profile-menu)
        tabs-item
        (menu-item :action (action
                             :name "Tabs..."
                             :handler tabs-window/make-tabs-window)
                   :id :tabs-menu)
        manage-menu (menu :text "Manage" :items [relays-item stats-item users-item profile-item tabs-item])
        menu-bar (menubar :items [manage-menu])]
    menu-bar))

(defn- make-main-window-title []
  (let [user-name (:name (get-mem :keys))
        title (str "More-Speech:" user-name " - " config/version)
        title (if (config/is-test-run?) (str title " - TEST") title)]
    title))

(defn- make-main-frame [messages-panel]
  (let [title (make-main-window-title)
        main-frame (frame :title title
                          :size [1000 :by 1000]
                          :menubar (make-menubar)
                          :content messages-panel)]
    (set-mem :frame main-frame)
    (listen main-frame :window-closing
            (fn [_]
              (let [send-chan (get-mem :send-chan)]
                (future (async/>!! send-chan [:closed])))
              (.dispose main-frame)))
    main-frame))

(defn make-main-window []
  (log-pr 2 'make-main-window)
  (let [article-area (article-panel/make-article-area)
        header-tab-panel (tabbed-panel :tabs (tabs/make-tabs) :id :header-tab-panel)
        article-panel (border-panel :north (article-panel/make-article-info-panel)
                                    :center (scrollable article-area)
                                    :south (article-panel/make-control-panel))
        messages-panel (top-bottom-split
                         header-tab-panel
                         article-panel
                         :divider-location 1/2)
        main-frame (make-main-frame messages-panel)]
    (show! main-frame)))

(defn repaint-main-window []
  (let [frame (get-mem :frame)]
    (when (some? frame)
      (when (get-mem :refresh-main-window)
        (set-mem :refresh-main-window false)
        (invoke-later (repaint! frame))))))

(defn setup-main-timer []
  (let [main-timer (Timer. "main timer")
        spec-task (proxy [TimerTask] []
                     (run [] (mem/conform-mem)))
        prune-tabs-task (proxy [TimerTask] []
                          (run [] (tabs/prune-tabs)))
        repaint-task (proxy [TimerTask] []
                       (run [] (repaint-main-window)))
        reload-article-task (proxy [TimerTask] []
                              (run [] (article-panel/reload-article)))
        prune-tabs-frequency (* config/prune-tabs-frequency-in-minutes 60 1000)]
    (.schedule main-timer
               prune-tabs-task
               (long prune-tabs-frequency)
               (long prune-tabs-frequency))
    (.schedule main-timer repaint-task 3000 3000)
    (.schedule main-timer reload-article-task 1000 1000)
    (when (config/is-test-run?)
      (.schedule main-timer spec-task 5000 5000))))

(defn setup-main-window []
  (setup-main-timer)
  (invoke-now (make-main-window))
  (log-pr 2 'setup-main-window 'creating-seesaw-handler)
  (->seesawHandler))





