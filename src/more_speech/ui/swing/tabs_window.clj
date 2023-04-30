(ns more-speech.ui.swing.tabs-window
  (:require
    [more-speech.config :as config]
    [more-speech.data-storage :as data-storage]
    [more-speech.db.gateway :as gateway]
    [more-speech.mem :refer :all]
    [more-speech.nostr.protocol :as protocol]
    [more-speech.ui.formatters :as formatters]
    [more-speech.ui.swing.article-panel :as article-panel]
    [more-speech.ui.swing.util :as swing-util])
  (:use (seesaw [core]))
  (:import (java.awt Point)
           (java.util Timer TimerTask)))

(defn close-tabs-frame [menu timer _e]
  (when-not (config/is-test-run?)
    (data-storage/write-tabs))
  (.cancel timer)
  (config! menu :enabled? true))

(defn repaint-tabs-window [frame]
  (.repaint frame))

(defn render-item [widget item]
  (let [id (:value item)
        event (gateway/get-event (config/get-db) id)
        rendered-text (if (some? event)
                        (formatters/format-header event :short)
                        (formatters/format-user-id id 70 40))
        pad (apply str (repeat 100 " "))]
    (config! widget
             :text (subs (str rendered-text pad) 0 100))))

(defn remove-id-from [listbox tab-name key id]
  (let [ids (remove #(= id %) (get-mem [:tabs-window tab-name key]))]
    (set-mem [:tabs-window tab-name key] ids)
    (config! listbox :model ids)))

(defn listbox-click [listbox tab-name key e]
  (let [index (.locationToIndex listbox (Point. (.getX e) (.getY e)))
        model (.getModel listbox)
        id (.getElementAt model index)
        event? (gateway/event-exists? (config/get-db) id)
        user? (some? (gateway/get-profile (config/get-db) id))
        select-string (if event? "Select" "Get Info...")
        enabled? (or event? user?)
        handler (if user?
                  (fn [_e] (article-panel/show-user-profile id))
                  (fn [_e] (swing-util/select-event id)))]
    (when (.isPopupTrigger e)
      (let [p (popup :items [(action :name select-string
                                     :enabled? enabled?
                                     :handler handler)
                             (action :name "Remove from tab"
                                     :handler (fn [_e]
                                                (swing-util/remove-id-from-tab tab-name key id)
                                                (remove-id-from listbox tab-name key id)))])]
        (.show p (to-widget e) (.x (.getPoint e)) (.y (.getPoint e)))))))

(defn make-tab [tab-desc]
  (let [tab-name (:name tab-desc)
        tab-label (label :text tab-name)
        selected-ids (:selected tab-desc)
        _ (set-mem [:tabs-window tab-name :selected] selected-ids)
        blocked-ids (:blocked tab-desc)
        _ (set-mem [:tabs-window tab-name :blocked] blocked-ids)
        all-ids (concat selected-ids blocked-ids)
        _ (protocol/request-profiles-and-contacts-for all-ids)
        _ (protocol/request-notes all-ids)
        selected-items selected-ids
        blocked-items blocked-ids
        selected-listbox (listbox :model selected-items :renderer render-item)
        blocked-listbox (listbox :model blocked-items :renderer render-item)
        selected-area (flow-panel :items [(label "Selected") (scrollable selected-listbox)])
        blocked-area (flow-panel :items [(label "Blocked") (scrollable blocked-listbox)])
        tab-window (vertical-panel :items [selected-area blocked-area])]
    (listen selected-listbox :mouse-pressed (partial listbox-click selected-listbox tab-name :selected))
    (listen blocked-listbox :mouse-pressed (partial listbox-click blocked-listbox tab-name :blocked))

    {:title tab-label
     :content tab-window}))

(defn make-tabs []
  (loop [tabs-list (remove #(= "all" (:name %)) (get-mem :tabs-list))
         header-tree-tabs []]
    (if (empty? tabs-list)
      header-tree-tabs
      (let [tab-data (make-tab (first tabs-list))]
        (recur (rest tabs-list)
               (conj header-tree-tabs tab-data))))))

(defn make-tabs-window [_e]
  (let [tabs-menu (select (get-mem :frame) [:#tabs-menu])
        _ (config! tabs-menu :enabled? false)
        tabs-frame (frame :title "Tabs Manager")
        tabs-window-timer (Timer. "Tabs window timer")
        tabs-window-repaint-task (proxy [TimerTask] []
                                   (run [] repaint-tabs-window tabs-frame))
        tab-panel (tabbed-panel :tabs (make-tabs) :id :tab-panel)
        ]
    (config! tabs-frame :content tab-panel)
    (listen tabs-frame :window-closing (partial close-tabs-frame tabs-menu tabs-window-timer))
    (.schedule tabs-window-timer tabs-window-repaint-task 1000 1000)
    (pack! tabs-frame)
    (show! tabs-frame)))
