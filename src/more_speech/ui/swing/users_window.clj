(ns more-speech.ui.swing.users-window
  (:require
    [clojure.set :as set]
    [more-speech.config :as config]
    [more-speech.config :refer [get-db]]
    [more-speech.db.gateway :as gateway]
    [more-speech.mem :refer :all]
    [more-speech.mem :refer :all]
    [more-speech.nostr.contact-list :as contact-list]
    [more-speech.nostr.event-composers :as composers]
    [more-speech.nostr.event-composers :as event-composers]
    [more-speech.nostr.protocol :as protocol]
    [more-speech.nostr.trust-updater :as trust-updater]
    [more-speech.nostr.util :as util]
    [more-speech.ui.formatters :as formatters]
    [more-speech.ui.swing.tabs :as tabs]
    [more-speech.ui.swing.user-info-interface :as user-info-interface]
    [more-speech.ui.swing.util :as swing-util])
  (:use (seesaw [core]))
  (:import (java.util Timer TimerTask)))

(defn close-users-frame [users-menu timer _e]
  (.cancel timer)
  (config! users-menu :enabled? true)
  (when (get-mem [:user-window :contact-list-changed])
    (when-not (config/is-test-run?)
      (let [my-pubkey (get-mem :pubkey)
            contacts (gateway/get-contacts (get-db) my-pubkey)]
        (event-composers/compose-and-send-contact-list contacts)))
    (future (protocol/reconnect-all-relays)))
  (set-mem :user-window nil))

(defn make-sorted-listbox-items [ids]
  (sort-by first
           (map #(vector (formatters/format-user-id % 70 40) %)
                ids)))

(defn render-listbox-item [widget item]
  (let [value (:value item)]
    (if (string? value)
      (text! widget value)
      (text! widget (formatters/format-user-id (second value) 70 40)))))

(defn load-recent-users []
  (let [after (- (util/get-now) (* 86400 1))
        recent-users (set/difference
                       (set (gateway/get-some-recent-event-authors (get-db) after))
                       (set (get-mem [:user-window :trusted-users])))
        recent-user-items (make-sorted-listbox-items recent-users)]
    (set-mem [:user-window :recent-users] recent-users)
    (set-mem [:user-window :recent-user-items] recent-user-items)
    (protocol/request-profiles-and-contacts-for recent-users)))

(defn load-trusted-users []
  (let [trusted-users (contact-list/get-trustees)]
    (set-mem [:user-window :trusted-users] trusted-users)
    (set-mem [:user-window :trusted-user-items]
             (make-sorted-listbox-items trusted-users))
    (protocol/request-profiles-and-contacts-for trusted-users)))

(defn load-web-of-trust-users []
  (let [web-of-trust-ids (contact-list/get-web-of-trust)
        reduced-web-of-trust-users (set/difference
                                     (set web-of-trust-ids)
                                     (set (get-mem [:user-window :trusted-users])))]
    (set-mem [:user-window :web-of-trust-users] reduced-web-of-trust-users)
    (set-mem [:user-window :web-of-trust-items]
             (make-sorted-listbox-items reduced-web-of-trust-users))
    (protocol/request-profiles-and-contacts-for reduced-web-of-trust-users)))

(defn load-user-window-data []
  (load-trusted-users)
  (if (= (get-mem [:user-window :selection-group]) :web-of-trust-items)
    (load-web-of-trust-users)
    (load-recent-users)))

(defn reload-user-window-data [frame]
  (load-user-window-data)
  (let [selected-listbox (select frame [:#selected-users])
        trusted-listbox (select frame [:#trusted-users-listbox])
        selected-group (get-mem [:user-window :selection-group])]
    (config! selected-listbox :model (get-mem [:user-window selected-group]))
    (config! trusted-listbox :model (get-mem [:user-window :trusted-user-items]))))

(defn select-recent-users [frame _e]
  (set-mem [:user-window :selection-group] :recent-user-items)
  (load-recent-users)
  (let [selected-listbox (select frame [:#selected-users])]
    (config! selected-listbox :model (get-mem [:user-window :recent-user-items]))))

(defn select-web-of-trust [frame _e]
  (set-mem [:user-window :selection-group] :web-of-trust-items)
  (when (empty? (get-mem [:user-window :web-of-trust-items]))
    (load-web-of-trust-users))
  (config! (select frame [:#selected-users])
           :model (get-mem [:user-window :web-of-trust-items])))

(defn remove-item [id items-tag ids-tag]
  (let [items (remove #(= id (second %)) (get-mem [:user-window items-tag]))
        ids (remove #(= id %) (get-mem [:user-window ids-tag]))]
    (set-mem [:user-window items-tag] items)
    (set-mem [:user-window ids-tag] ids)))

(defn find-item [id items]
  (loop [items items]
    (if (empty? items)
      nil
      (let [item (first items)]
        (if (= id (second item))
          item
          (recur (rest items)))))))

(defn trust-selection [frame _e]
  (let [selected-listbox (select frame [:#selected-users])
        selected-item (selection selected-listbox)
        group (get-mem [:user-window :selection-group])]
    (when (some? selected-item)
      (let [pubkey (second selected-item)
            petname (trust-updater/ask-for-petname pubkey)
            trusted-listbox (select frame [:#trusted-users-listbox])]
        (when (some? petname)
          (trust-updater/entrust pubkey petname)
          (set-mem [:user-window :contact-list-changed] true)
          (load-trusted-users)
          (let [trusted-items (get-mem [:user-window :trusted-user-items])
                new-trusted-item (find-item pubkey trusted-items)]
            (config! trusted-listbox :model trusted-items)
            (remove-item pubkey :web-of-trust-items :web-of-trust-users)
            (remove-item pubkey :recent-user-items :recent-users)
            (config! (select frame [:#selected-users])
                     :model (get-mem [:user-window group]))
            (.setSelectedValue trusted-listbox
                               new-trusted-item true)))))))

(defn untrust-items [listbox-items]
  (loop [items listbox-items]
    (when-let [item (first items)]
      (let [untrusted-user (second item)]
        (trust-updater/untrust untrusted-user)
        (recur (rest items))))))

(defn add-items-to-recent-users [listbox-items]
  (loop [items listbox-items]
    (when-let [item (first items)]
      (let [user-id (second item)]
        (update-mem [:user-window :recent-users] conj user-id)
        (recur (rest items))))))

(defn untrust-selection [frame _e]
  (let [trusted-listbox (select frame [:#trusted-users-listbox])
        recent-button (select frame [:#recent-button])
        selected-listbox (select frame [:#selected-users])
        selected-items (selection trusted-listbox {:multi? true})]
    (untrust-items selected-items)
    (load-trusted-users)
    (config! trusted-listbox :model (get-mem [:user-window :trusted-user-items]))
    (config! recent-button :selected? true)
    (load-recent-users)
    (add-items-to-recent-users selected-items)
    (set-mem [:user-window :recent-user-items]
             (make-sorted-listbox-items
               (get-mem [:user-window :recent-users])))
    (config! selected-listbox :model (get-mem [:user-window :recent-user-items]))
    (set-mem [:user-window :contact-list-changed] true)
    (composers/compose-and-send-contact-list
      (gateway/get-contacts (get-db) (get-mem :pubkey)))))

(defn listbox-click [e]
  (let [item (swing-util/get-clicked-value e)
        user-id (second item)
        tab-names (vec (remove #(= "all" %) (map :name (get-mem :tabs-list))))
        tab-names (conj tab-names "<new-tab>")
        add-author-actions (map #(action :name % :handler (partial tabs/add-author-to-tab user-id %)) tab-names)]
    (protocol/request-profiles-and-contacts-for user-id)
    (when (.isPopupTrigger e)
      (let [p (popup :items [(action :name "Get Info..." :handler (fn [_e] (user-info-interface/show-user-profile user-id)))
                             (menu :text "Add author to tab" :items add-author-actions)])]
        (swing-util/show-popup p e)))))

(defn- repaint-user-window [frame]
  (.repaint frame))

(defn- make-trusted-users-listbox []
  (let [trusted-users-listbox
        (listbox :id :trusted-users-listbox
                 :font config/default-font
                 :renderer render-listbox-item
                 :model (get-mem [:user-window :trusted-user-items]))]
    (listen trusted-users-listbox :mouse-pressed listbox-click)
    trusted-users-listbox))

(defn- make-trusted-users-panel []
  (vertical-panel
    :items [(label "Trusted")
            (scrollable (make-trusted-users-listbox) :size [500 :by 800])]))

(defn make-operations-panel [users-frame]
  (let [trust-button (button :text "<-Trust"
                             :listen [:action (partial trust-selection users-frame)])
        untrust-button (button :text "Untrust->"
                               :listen [:action (partial untrust-selection users-frame)])
        reload-button (button :text "Reload"
                              :listen [:action (fn [_e] (reload-user-window-data users-frame))])
        operations-panel (vertical-panel :items [trust-button
                                                 untrust-button
                                                 reload-button])]
    operations-panel))

(defn- make-selection-panel [users-frame]
  (let [selection-group (button-group)
        web-of-trust-button (radio :text "Web of trust"
                                   :group selection-group
                                   :selected? false
                                   :listen [:action (partial select-web-of-trust users-frame)])
        recent-button (radio :text "Recent users"
                             :id :recent-button
                             :group selection-group
                             :selected? true
                             :listen [:action (partial select-recent-users users-frame)])
        selected-listbox (listbox :id :selected-users
                                  :font config/default-font
                                  :renderer render-listbox-item)
        scrollable-selected-listbox (scrollable selected-listbox
                                                :size [500 :by 800])
        selection-panel (vertical-panel :items [web-of-trust-button
                                                recent-button
                                                scrollable-selected-listbox])]
    (config! selected-listbox :model (get-mem [:user-window :recent-user-items]))
    (listen selected-listbox :mouse-pressed listbox-click)
    selection-panel))

(defn make-users-frame [_e]
  (load-user-window-data)
  (let [users-menu (select (get-mem :frame) [:#users-menu])
        users-frame (frame :title "Users")
        trusted-users-panel (make-trusted-users-panel)
        operations-panel (make-operations-panel users-frame)
        selection-panel (make-selection-panel users-frame)
        users-panel (horizontal-panel :items [trusted-users-panel
                                              operations-panel
                                              selection-panel])
        user-window-timer (Timer. "User window timer")
        user-window-repaint-task (proxy [TimerTask] []
                                   (run [] (repaint-user-window users-frame)))]
    (set-mem [:user-window :selection-group] :recent-user-items)
    (config! users-frame :content users-panel)
    (listen users-frame :window-closing (partial close-users-frame users-menu user-window-timer))
    (config! users-menu :enabled? false)
    (.schedule user-window-timer user-window-repaint-task 1000 1000)
    (pack! users-frame)
    (show! users-frame)))
