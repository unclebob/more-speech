(ns more-speech.ui.swing.relay-manager
  (:use [seesaw core])
  (:require [more-speech.mem :refer :all]
            [more-speech.nostr.protocol :as protocol]
            [more-speech.config :as config]
            [more-speech.nostr.util :as util]))

(def manager-width 800)
(def manager-height 500)
(def element-height 50)
(def field-height 20)
(def url-height 45)

(defn reconnect-to-relay [url]
  (let [relay (get-in @relays [url :connection])
        relay (if (some? relay) relay (protocol/make-relay url))
        now (util/get-now)]
    (swap! relays assoc-in [url :retries] 0)
    (-> relay protocol/close-relay protocol/connect-to-relay)
    (protocol/subscribe-to-relay url config/subscription-id-base (- now 3600) now)
    ))

(defn set-relay-read [label url read-type _e]
  (swap! relays assoc-in [url :read] read-type)
  (config! label :text (str read-type))
  (reconnect-to-relay url))

(defn set-relay-write [label url write-type _e]
  (swap! relays assoc-in [url :write] write-type)
  (config! label :text (str write-type))
  (reconnect-to-relay url))

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

(defn valid-relay-url? [url]
  (let [prefix (re-find config/relay-pattern url)]
    (and (re-matches config/url-pattern url)
         (some? prefix)
         (.startsWith url prefix))))

(defn commit-url [field new-url old-url]
  (if-not (valid-relay-url? new-url)
    (do
      (config! field :text old-url)
      (when (nil? old-url)
        (config! field :foreground :darkgrey)
        (alert (str new-url " is invalid."))))
    (swap! relays assoc new-url {:read :read-none :write false})))

(defn key-pressed-in-name [url e]
  (let [char (.getKeyChar e)
        field (.getComponent e)]
    (if (= char \newline)
      (config! field :foreground :black)
      (config! field :foreground :darkgrey))
    (when (= char \newline)
      (let [new-url (.trim (config field :text))]
        (commit-url field new-url url)))))

(defn mouse-pressed-in-name [url e]
  (let [field (.getComponent e)]
    (when (and (nil? url)
               (.startsWith (config field :text) "<"))
      (config! field :text ""))))

(defn is-connected? [url]
  (some? (get-in @relays [url :connection])))

(defn make-relay-element
  ([url]
   (let [relay (get @relays url)
         relay-name (re-find config/relay-pattern url)
         connection-mark (if (is-connected? url) "✓" "X")
         write-status (str (:write relay))
         read-status (str (:read relay))]
     (make-relay-element url relay-name connection-mark read-status write-status)))

  ([url relay-name connection-mark read-status write-status]
   (let [name-field (text :text relay-name :size [450 :by element-height]
                          :font :monospaced :editable? true :multi-line? true :wrap-lines? true
                          :id :relay-name :user-data relay-name)
         connection-label (label :text connection-mark :size [10 :by field-height])
         read-label (text :text read-status :editable? false :size [100 :by field-height])
         write-label (text :text write-status :size [50 :by field-height])
         element (horizontal-panel :size [manager-width :by element-height]
                                   :border (seesaw.border/line-border)
                                   :items [name-field connection-label read-label write-label])]
     (listen read-label :mouse-pressed (partial read-click url))
     (listen write-label :mouse-pressed (partial write-click url))
     (listen name-field :key-pressed (partial key-pressed-in-name url))
     (listen name-field :mouse-pressed (partial mouse-pressed-in-name url))
     element)))

(defn show-relay-manager [_e]
  (let [relay-frame (frame :title "Relays" :size [manager-width :by manager-height])
        all-relay-urls (set (keys @relays))
        active-urls (sort (filter protocol/is-active-url? all-relay-urls))
        inactive-urls (sort (remove protocol/is-active-url? all-relay-urls))
        connected-elements (map make-relay-element active-urls)
        unconnected-elements (map make-relay-element inactive-urls)
        new-element (make-relay-element nil "<add-relay>" "X" ":read-none" "false")
        _ (config! (select new-element [:#relay-name]) :foreground :darkgrey)
        all-elements (concat [new-element] connected-elements unconnected-elements)
        relay-box (scrollable (vertical-panel :items all-elements))
        ]
    (config! relay-frame :content relay-box)
    (show! relay-frame)
    (scroll! relay-box :to :top)))

