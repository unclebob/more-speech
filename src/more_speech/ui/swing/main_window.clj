(ns more-speech.ui.swing.main-window
  (:require [clojure.core.async :as async]
            [clojure.java.browse :as browse]
            [more-speech.db.gateway :as gateway]
            [more-speech.nostr.event-handlers :as handlers]
            [more-speech.ui.swing.article-tree :as article-tree]
            [more-speech.ui.swing.article-panel :as article-panel]
            [more-speech.ui.swing.tabs :as tabs]
            [more-speech.ui.swing.util :as swing-util]
            [more-speech.mem :refer :all]
            [more-speech.config :as config :refer [get-db]]
            [more-speech.ui.formatter-util :as formatter-util]
            [more-speech.nostr.util :as util]
            [clojure.string :as string]
            [clojure.set :as set]
            [more-speech.nostr.protocol :as protocol])
  (:use [seesaw core])
  (:import (javax.swing Timer)
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

(defn timer-action [_]
  ;nothing for now.
  )

(defn make-profile-line [id]
  (let [profile (gateway/get-profile (get-db) id)
        name (formatter-util/abbreviate (:name profile) 20)]
    (format "%-20s %s %s" name (util/num32->hex-string id) (:picture profile))))

(defn reconnect-to-relay [url]
  (let [relay (get-in @relays [url :connection])
        relay (if (some? relay) relay (protocol/make-relay url))
        now (util/get-now)]
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

(defn is-connected? [url]
  (some? (get-in @relays [url :connection])))

(defn make-relay-element [url]
  (let [relay (get @relays url)
        name (label :text (re-find config/relay-pattern url) :size [250 :by 20])
        connection-label (label :text (if (is-connected? url) "✓" "X") :size [10 :by 20])
        read-label (text :text (str (:read relay)) :editable? false :size [100 :by 20])
        write-label (text :text (str (:write relay)) :size [40 :by 20])
        element (horizontal-panel :size [500 :by 20] :items [name connection-label read-label write-label])]
    (listen read-label :mouse-pressed (partial read-click url))
    (listen write-label :mouse-pressed (partial write-click url))
    element))

(defn show-relays [_e]
  (let [relay-frame (frame :title "Relays" :size [500 :by 500])
        all-relay-urls (set (keys @relays))
        connected-relays (set (filter #(or (not= :read-none (:read (get @relays %)))
                                           (:write (get @relays %))) all-relay-urls))
        unconnected-relays (set/difference all-relay-urls connected-relays)
        connected-elements (map make-relay-element connected-relays)
        unconnected-elements (map make-relay-element unconnected-relays)
        relay-box (scrollable (vertical-panel :items (concat connected-elements unconnected-elements)))
        ]
    (config! relay-frame :content relay-box)
    (show! relay-frame)))

(defn make-menubar []
  (let [relays-item (menu-item :action (action :name "Relays..." :handler show-relays))
        users-item (menu-item :text "Users...")
        profile-item (menu-item :text "Profile...")
        manage-menu (menu :text "Manage" :items [relays-item users-item profile-item])
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
        _ (prn 'make-main-window 'messages-panel-complete)
        timer (Timer. 100 nil)]
    (config! main-frame :content messages-panel)
    (listen timer :action timer-action)
    (listen main-frame :window-closing
            (fn [_]
              (.stop timer)
              (let [send-chan (get-mem :send-chan)]
                (future (async/>!! send-chan [:closed])))
              (.dispose main-frame)))
    (prn 'make-main-window 'showing-main-frame)
    (show! main-frame)
    (prn 'make-main-window 'shown)
    (.start timer)))

(defn setup-main-window []
  (invoke-now (make-main-window))
  (prn 'setup-main-window 'creating-seesaw-handler)
  (->seesawHandler))





