(ns more-speech.ui.swing.tabs-window
  (:require
    [more-speech.config :as config]
    [more-speech.data-storage :as data-storage]
    [more-speech.db.gateway :as gateway]
    [more-speech.mem :refer :all]
    [more-speech.nostr.protocol :as protocol]
    [more-speech.ui.formatters :as formatters]
    [more-speech.ui.swing.user-info-interface :as html-interface]
    [more-speech.ui.swing.util :as swing-util])
  (:use (seesaw [core]))
  (:import (java.util Timer TimerTask)))

(defn close-tabs-frame [menu timer _e]
  (data-storage/write-tabs)
  (set-mem :tabs-window nil)
  (.cancel timer)
  (config! menu :enabled? true))

(defn repaint-tabs-window [frame]
  (.repaint frame))

(defn- render-id [widget id]
  (let [event (gateway/get-event (config/get-db) id)
        rendered-text (if (some? event)
                        (formatters/format-header event :menu-item)
                        (formatters/format-user-id id 70 40))
        pad (apply str (repeat 100 " "))]
    (config! widget
             :text (subs (str rendered-text pad) 0 100))))

(defn- render-regex [widget regex]
  (config! widget :text (str "match:" regex)))

(defn render-item [widget item]
  (let [item-value (:value item)]
    (cond
      (number? item-value)
      (render-id widget item-value)

      (string? item-value)
      (render-regex widget item-value))))

(defn remove-id-from [listbox tab-name key id]
  (let [ids (remove #(= id %) (get-mem [:tabs-window tab-name key]))]
    (set-mem [:tabs-window tab-name key] ids)
    (config! listbox :model ids)))

(defn listbox-click [listbox tab-name key e]
  (let [id (swing-util/get-clicked-value e)
        event? (gateway/event-exists? (config/get-db) id)
        user? (some? (gateway/get-profile (config/get-db) id))
        select-string (if event? "Select" "Get Info...")
        enabled? (or event? user?)
        handler (if user?
                  (fn [_e] (html-interface/show-user-profile id))
                  (fn [_e] (swing-util/select-event id)))]
    (when (.isPopupTrigger e)
      (let [p (popup :items [(action :name select-string
                                     :enabled? enabled?
                                     :handler handler)
                             (action :name "Remove from tab"
                                     :handler (fn [_e]
                                                (swing-util/remove-id-from-tab tab-name key id)
                                                (remove-id-from listbox tab-name key id)))])]
        (swing-util/show-popup p e)))))

(defn- make-selected-area [tab-desc]
  (let [tab-name (:name tab-desc)
        selected-filters (filter #(or (number? %) (string? %))
                                 (:selected tab-desc))
        selected-listbox (listbox :model selected-filters :renderer render-item)
        selected-area (flow-panel :items [(label "Selected") (scrollable selected-listbox)])]
    (set-mem [:tabs-window tab-name :selected] selected-filters)
    (set-mem [:tabs-window tab-name :selected-listbox] selected-listbox)
    (listen selected-listbox :mouse-pressed (partial listbox-click selected-listbox tab-name :selected))
    selected-area))

(defn- make-blocked-area [tab-desc]
  (let [tab-name (:name tab-desc)
        blocked-ids (:blocked tab-desc)
        blocked-listbox (listbox :model blocked-ids :renderer render-item)
        blocked-area (flow-panel :items [(label "Blocked") (scrollable blocked-listbox)])]
    (set-mem [:tabs-window tab-name :blocked] blocked-ids)
    (listen blocked-listbox :mouse-pressed (partial listbox-click blocked-listbox tab-name :blocked))
    blocked-area)
  )

(defn- regex-field-key [tab-desc key-event]
  (let [tab-name (:name tab-desc)
        regex-field (.getComponent key-event)
        c (.getKeyChar key-event)
        regex (text regex-field)]
    (when (= \newline c)
      (text! regex-field "")
      (update-mem [:tabs-window tab-name :selected] conj regex)
      (config! (get-mem [:tabs-window tab-name :selected-listbox])
               :model (get-mem [:tabs-window tab-name :selected]))
      (swing-util/add-filter-to-tab tab-name :selected regex)))
  )

(defn- make-regex-area [tab-desc]
  (let [regex-field (text :editable? true :columns 40
                          :listen [:key-pressed (partial regex-field-key tab-desc)])
        regex-area (flow-panel :items ["Selection pattern:" regex-field])]
    regex-area))

(defn make-tab [tab-desc]
  (let [tab-name (:name tab-desc)
        tab-label (label :text tab-name)
        all-ids (filter number? (concat (:selected tab-desc) (:blocked tab-desc)))
        tab-window (vertical-panel
                     :items [(make-selected-area tab-desc)
                             (make-regex-area tab-desc)
                             (make-blocked-area tab-desc)])]
    (set-mem [:tabs-window :all-ids]
             (concat (get-mem [:tabs-window :all-ids]) all-ids))
    {:title tab-label
     :content tab-window}))

(defn make-tabs []
  (loop [tabs-list (remove #(= "all" (:name %)) (get-mem :tabs-list))
         header-tree-tabs []]
    (if (empty? tabs-list)
      (let [all-ids (get-mem [:tabs-window :all-ids])]
        (protocol/request-profiles-and-contacts-for all-ids)
        (protocol/request-notes all-ids)
        header-tree-tabs)
      (let [tab-data (make-tab (first tabs-list))]
        (recur (rest tabs-list)
               (conj header-tree-tabs tab-data)))))
  )

(defn make-tabs-window [_e]
  (let [tabs-menu (select (get-mem :frame) [:#tabs-menu])
        _ (config! tabs-menu :enabled? false)
        tabs-frame (frame :title "Tabs Manager")
        tabs-window-timer (Timer. "Tabs window timer")
        tabs-window-repaint-task (proxy [TimerTask] []
                                   (run [] (repaint-tabs-window tabs-frame)))
        tab-panel (tabbed-panel :tabs (make-tabs) :id :tab-panel)]
    (config! tabs-frame :content tab-panel)
    (listen tabs-frame :window-closing (partial close-tabs-frame tabs-menu tabs-window-timer))
    (.schedule tabs-window-timer tabs-window-repaint-task 1000 1000)
    (pack! tabs-frame)
    (show! tabs-frame)))
