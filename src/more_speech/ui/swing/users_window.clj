(ns more-speech.ui.swing.users-window
  (:require
    [clojure.set :as set]
    [more-speech.config :as config]
    [more-speech.config :refer [get-db]]
    [more-speech.db.gateway :as gateway]
    [more-speech.mem :refer :all]
    [more-speech.mem :refer :all]
    [more-speech.nostr.contact-list :as contact-list]
    [more-speech.nostr.util :as util]
    [more-speech.ui.formatters :as formatters])
  (:use (seesaw [core])))

(defn close-users-frame [users-menu _e]
  (config! users-menu :enabled? true)
  (set-mem :user-window nil))

(defn make-sorted-listbox-items [ids]
  (sort-by first
           (map #(vector (formatters/format-user-id % 70 40) %)
                ids))
  )

(defn render-listbox-item [widget item]
  (let [value (:value item)]
    (if (string? value)
      (text! widget value)
      (text! widget (first value)))))

(defn trust-selection [frame _e]
  (let [selected-listbox (select frame [:#selected-users])
        selected-item (selection selected-listbox)]
    (when (some? selected-item)
      )))

(defn load-web-of-trust-authors [frame  _e]
  (config! (select frame [:#selected-users])
           :model (get-mem [:user-window :web-of-trust-items])))

(defn load-recent-users [frame _e]
  (let [after (- (util/get-now) (* 86400 1))
        recent-users (set/difference
                         (set (gateway/get-some-recent-event-authors (get-db) after))
                         (set (get-mem [:user-window :trusted-users])))
        recent-user-items (make-sorted-listbox-items recent-users)
        selected-listbox (select frame [:#selected-users])]
    (set-mem [:user-window :recent-users] recent-users)
    (set-mem [:user-window :recent-user-items] recent-user-items)
    (config! selected-listbox :model recent-user-items)))

(defn load-trusted-users []
  (let [trusted-users (contact-list/get-trustees)]
    (set-mem [:user-window :trusted-users] trusted-users)
    (set-mem [:user-window :trusted-user-items]
             (make-sorted-listbox-items trusted-users))))

(defn load-web-of-trust-users []
  (let [web-of-trust-users (set/difference
                             (set (contact-list/get-web-of-trust))
                             (set (get-mem [:user-window :trusted-users])))]
    (set-mem [:user-window :web-of-trust-users] web-of-trust-users)
    (set-mem [:user-window :web-of-trust-items]
             (make-sorted-listbox-items web-of-trust-users))))

  (defn load-user-window-data []
    (load-trusted-users)
    (load-web-of-trust-users))

  (defn make-users-frame [_e]
    (let [users-menu (select (get-mem :frame) [:#users-menu])
          users-frame (frame :title "Users")
          trusted-users-listbox (listbox :id :trusted-users-listbox
                                         :font config/default-font
                                         :renderer render-listbox-item)
          trusted-users-panel (vertical-panel
                                :items [(label "Trusted")
                                        (scrollable trusted-users-listbox :size [500 :by 800])])
          trust-button (button :text "<-"
                               :listen [:action (partial trust-selection users-frame)])
          selection-group (button-group)
          web-of-trust-button (radio :text "Web of trust"
                                     :group selection-group
                                     :selected? true
                                     :listen [:action (partial load-web-of-trust-authors users-frame)])
          recent-button (radio :text "Recent users"
                               :group selection-group
                               :listen [:action (partial load-recent-users users-frame)])

          search-field (text :text "" :editable? true)
          selected-listbox (listbox :id :selected-users
                                    :font config/default-font
                                    :renderer render-listbox-item)
          scrollable-selected-listbox (scrollable selected-listbox
                                                  :size [500 :by 800])
          selection-panel (vertical-panel :items [web-of-trust-button
                                                  recent-button
                                                  search-field
                                                  scrollable-selected-listbox])


          users-panel (horizontal-panel :items [trusted-users-panel trust-button selection-panel])]
      (load-user-window-data)
      (config! trusted-users-listbox :model (get-mem [:user-window :trusted-user-items]))
      (config! selected-listbox :model (get-mem [:user-window :web-of-trust-items]))
      (config! users-frame :content users-panel)
      (listen users-frame :window-closing (partial close-users-frame users-menu))
      (config! users-menu :enabled? false)
      (pack! users-frame)
      (show! users-frame)))
