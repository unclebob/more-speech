(ns more-speech.ui.swing.relay-manager
  (:use [seesaw core])
  (:require [more-speech.mem :refer :all]
            [more-speech.nostr.protocol :as protocol]
            [more-speech.config :as config]
            [more-speech.nostr.util :as util])
  (:import (java.util Timer TimerTask)))

(def manager-width 800)
(def manager-height 500)
(def element-height 50)
(def field-height 20)
(def url-height 45)
(def name-width 450)
(def info-width (- manager-width name-width))

;---CALLBACK DECLARATIONS
(declare close-relay-manager read-click write-click key-pressed-in-name mouse-pressed-in-name)

(defn reconnect-to-relay [url]
  (let [relay (get-in @relays [url :connection])
        relay (if (some? relay) relay (protocol/make-relay url))
        now (util/get-now)]
    (swap! relays assoc-in [url :retries] 0)
    (protocol/close-relay relay)
    (protocol/connect-to-relay relay)
    (protocol/subscribe-to-relay url (- now 3600) now)
    ))

(defn set-relay-read [label url read-type _e]
  (swap! relays assoc-in [url :read] read-type)
  (config! label :text (str read-type))
  (reconnect-to-relay url))

(defn set-relay-write [label url write-type _e]
  (swap! relays assoc-in [url :write] write-type)
  (config! label :text (str write-type))
  (reconnect-to-relay url))

(defn is-connected? [url]
  (some? (get-in @relays [url :connection])))

(defn relay-id
  "converts a url to a string suitable for a select id"
  [url]
  (if (nil? url)
    ""
    (let [dns-and-stuff (re-find config/relay-pattern url)]
      (if (nil? dns-and-stuff)
        ""
        (let [last-stuff (.lastIndexOf dns-and-stuff "/")]
          (apply str (remove #(= \. %) (subs dns-and-stuff (inc last-stuff)))))))))

(defn make-relay-element
  ([url]
   (let [relay (get @relays url)
         relay-name url
         connection-mark (if (is-connected? url) "✓" "X")
         write-status (str (:write relay))
         read-status (str (:read relay))]
     (make-relay-element url relay-name connection-mark read-status write-status)))

  ([url relay-name connection-mark read-status write-status]
   (let [name-field (text :text relay-name :size [name-width :by element-height]
                          :font :monospaced :editable? true :multi-line? true :wrap-lines? true
                          :id :relay-name)
         connection-label-id (keyword (str "connection-" (relay-id relay-name)))
         connection-label (label :text connection-mark :size [10 :by field-height]
                                 :id connection-label-id)
         read-label (text :text read-status :editable? false :size [100 :by field-height])
         write-label (text :text write-status :size [50 :by field-height])
         relay-event-counter-id (keyword (str "events-" (relay-id relay-name)))
         events-label (label :text "" :size [60 :by field-height] :id relay-event-counter-id)
         notice-label-id (keyword (str "notice-" (relay-id relay-name)))
         notice-label (label :text (get-mem [:relay-notice url])
                             :size [info-width :by field-height]
                             :id notice-label-id
                             :halign :left)
         status-bar (horizontal-panel :size [info-width :by field-height]
                                      :border 0
                                      :items [connection-label read-label write-label events-label])
         info-area (border-panel :north status-bar :south notice-label :drag-enabled? false :border 0)
         element (left-right-split name-field info-area
                                   :size [manager-width :by element-height]
                                   :drag-enabled? false
                                   :border (seesaw.border/line-border))]
     (listen read-label :mouse-pressed (partial read-click url))
     (listen write-label :mouse-pressed (partial write-click url))
     (listen name-field :key-pressed (partial key-pressed-in-name url))
     (listen name-field :mouse-pressed (partial mouse-pressed-in-name url))
     element)))

(defn valid-relay-url? [url]
  (let [prefix (re-find config/relay-pattern url)]
    (and (re-matches config/url-pattern url)
         (some? prefix)
         (.startsWith url prefix))))

(defn make-add-relay-element []
  (let [add-relay-element (make-relay-element nil "<add-relay>" "X" ":read-none" "false")]
    (config! (select add-relay-element [:#relay-name])
             :foreground :darkgrey)
    add-relay-element))

(defn get-all-relay-elements []
  (let [relay-frame (get-mem :relay-manager-frame)
        relay-list (select relay-frame [:#relay-list])
        relay-elements (config relay-list :items)]
    relay-elements)
  )

(defn replace-element-in-manager-frame [new-url old-url]
  (let [relay-frame (get-mem :relay-manager-frame)
        relay-list (select relay-frame [:#relay-list])
        relay-elements (config relay-list :items)]
    (loop [elements relay-elements
           pruned-elements []]
      (if (empty? elements)
        (do
          (if (nil? old-url)
            (config! relay-list :items (concat [(make-add-relay-element)] pruned-elements))
            (config! relay-list :items pruned-elements)))
        (let [element (first elements)
              name-field (select element [:#relay-name])
              name (config name-field :text)]
          (cond
            (empty? name)
            (recur (rest elements) pruned-elements)

            (= name new-url)
            (recur (rest elements) (conj pruned-elements (make-relay-element new-url)))

            :else
            (recur (rest elements) (conj pruned-elements element))))))))

(defn delete-url [url]
  (swap! relays dissoc url)
  (replace-element-in-manager-frame nil url))

(defn commit-valid-url [new-url old-url]
  (if (some? old-url)
    (swap! relays assoc new-url (get @relays old-url))
    (swap! relays assoc new-url {:read :read-none :write false}))
  (when (some? old-url)
    (swap! relays dissoc old-url))
  (replace-element-in-manager-frame new-url old-url))

(defn revert-field [field old-url]
  (config! field :text old-url)
  (when (nil? old-url)
    (config! field :foreground :darkgrey)))

(defn commit-url [field new-url old-url]
  (if (empty? new-url)
    (do
      (if (confirm (str "Delete " old-url "?"))
        (delete-url old-url)
        (revert-field field old-url)))
    (if-not (valid-relay-url? new-url)
      (do
        (revert-field field old-url)
        (alert (str new-url " is invalid.")))
      (commit-valid-url new-url old-url))))

(defn show-relay-status [relay-panel]
  (let [urls (keys @relays)
        active-urls (sort (filter protocol/is-active-url? urls))]
    (doseq [url active-urls]
      (let [event-counter-selector (keyword (str "#events-" (relay-id url)))
            event-label (select relay-panel [event-counter-selector])
            notice-label-selector (keyword (str "#notice-" (relay-id url)))
            notice-label (select relay-panel [notice-label-selector])
            connection-label-selector (keyword (str "#connection-" (relay-id url)))
            connection-label (select relay-panel [connection-label-selector])
            ]
        (config! event-label
                 :text (str (get-mem [:events-by-relay url])))
        (config! notice-label
                 :text (get-mem [:relay-notice url]))
        (config! connection-label
                 :text (if (is-connected? url) "✓" "X"))
        ))
    ))

(defn show-relay-manager [_e]
  (when-not (get-mem :relay-manager-frame)
    (let [relay-frame (frame :title "Relays" :size [manager-width :by manager-height])
          all-relay-urls (set (keys @relays))
          active-urls (sort (filter protocol/is-active-url? all-relay-urls))
          inactive-urls (sort (remove protocol/is-active-url? all-relay-urls))
          connected-elements (map make-relay-element active-urls)
          unconnected-elements (map make-relay-element inactive-urls)
          new-element (make-add-relay-element)
          all-elements (concat [new-element] connected-elements unconnected-elements)
          relay-list (vertical-panel :items all-elements :id :relay-list)
          relay-box (scrollable relay-list)
          relays-menu (select (get-mem :frame) [:#relays-menu])
          relay-manager-timer (Timer. "Relay manager timer")
          relay-manager-task (proxy [TimerTask] []
                               (run [] (show-relay-status relay-list)))
          ]
      (config! relays-menu :enabled? false)
      (set-mem :relay-manager-frame relay-frame)
      (config! relay-frame :content relay-box)
      (listen relay-frame :window-closing close-relay-manager)
      (show! relay-frame)
      (.schedule relay-manager-timer relay-manager-task 1000 1000)
      )))


;---------CALLBACKS

(defn key-pressed-in-name [url e]
  (let [char (.getKeyChar e)
        field (.getComponent e)]
    (if (= char \newline)
      (config! field :foreground :black)
      (config! field :foreground :darkgrey))
    (when (= char \newline)
      (let [new-url (.trim (config field :text))]
        (when (not= new-url url)
          (commit-url field new-url url))))))

(defn mouse-pressed-in-name [url e]
  (let [field (.getComponent e)]
    (when (and (nil? url)
               (.startsWith (config field :text) "<"))
      (config! field :text "wss://"))))

(defn close-relay-manager [_e]
  (set-mem :relay-manager-frame nil)
  (let [relays-menu (select (get-mem :frame) [:#relays-menu])]
    (config! relays-menu :enabled? true)))

(defn read-click [url e]
  (when (.isPopupTrigger e)
    (let [x (.x (.getPoint e))
          y (.y (.getPoint e))
          label (.getComponent e)
          p (popup :items [(action :name "none" :handler (partial set-relay-read label url :read-none))
                           (action :name "all" :handler (partial set-relay-read label url :read-all))
                           (action :name "trusted" :handler (partial set-relay-read label url :read-trusted))
                           (action :name "web of trust" :handler (partial set-relay-read label url :read-web-of-trust))])]
      (.show p (to-widget e) x y))))

(defn write-click [url e]
  (when (.isPopupTrigger e)
    (let [x (.x (.getPoint e))
          y (.y (.getPoint e))
          label (.getComponent e)
          p (popup :items [(action :name "true" :handler (partial set-relay-write label url true))
                           (action :name "false" :handler (partial set-relay-write label url false))
                           ])]
      (.show p (to-widget e) x y))))

