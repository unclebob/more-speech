(ns more-speech.ui.swing.users-window
  (:require
    [more-speech.config :as config]
    [more-speech.config :refer [get-db]]
    [more-speech.db.gateway :as gateway]
    [more-speech.mem :refer :all]
    [more-speech.nostr.contact-list :as contact-list]
    [more-speech.nostr.util :as util]
    [more-speech.ui.formatters :as formatters])
  (:use (seesaw [core])))

(defn close-users-frame [users-menu _e]
  (config! users-menu :enabled? true))

(defn make-sorted-listbox-items [ids]
  (sort-by first
           (map #(vector (formatters/format-user-id % 70 40) %)
                ids))
  )

(defn load-recent-authors [frame _e]
  (let [after (- (util/get-now) (* 86400 1))
        recent-authors (gateway/get-some-recent-event-authors (get-db) after)
        recent-author-items (make-sorted-listbox-items recent-authors)
        selected-listbox (select frame [:#selected-users])]
    (config! selected-listbox :model recent-author-items)))

(defn load-web-of-trust-authors [frame authors _e]
  (config! (select frame [:#selected-users]) :model authors))

(defn render-listbox-item [widget item]
  (let [value (:value item)]
    (if (string? value)
      (text! widget value)
      (text! widget (first value)))))

(defn make-users-frame [_e]
  (let [users-menu (select (get-mem :frame) [:#users-menu])
        users-frame (frame :title "Users")
        trusted-users-listbox (listbox :id :trusted-users-listbox
                                       :font config/default-font
                                       :renderer render-listbox-item)
        trusted-users (contact-list/get-trustees)
        trusted-user-items (make-sorted-listbox-items trusted-users)
        web-of-trust-users (contact-list/get-web-of-trust)
        web-of-trust-items (make-sorted-listbox-items web-of-trust-users)
        trusted-users-panel (vertical-panel
                              :items [(label "Trusted")
                                      (scrollable trusted-users-listbox :size [500 :by 800])])
        trust-button (button :text "<-")
        selection-group (button-group)
        web-of-trust-button (radio :text "Web of trust"
                                   :group selection-group
                                   :selected? true
                                   :listen [:action (partial load-web-of-trust-authors users-frame web-of-trust-items)])
        recent-button (radio :text "Recent users"
                             :group selection-group
                             :listen [:action (partial load-recent-authors users-frame)])

        search-field (text :text "" :editable? true)
        selected-listbox (listbox :id :selected-users
                                  :font config/default-font
                                  :renderer render-listbox-item)
        scrollable-selected-listbox (scrollable selected-listbox
                                                :size [500 :by 800])
        selection-panel (vertical-panel :items [web-of-trust-button
                                                recent-button
                                                search-field
                                                scrollable-selected-listbox]
                                        )

        users-panel (horizontal-panel :items [trusted-users-panel trust-button selection-panel])]
    (config! trusted-users-listbox :model trusted-user-items)
    (config! selected-listbox :model web-of-trust-items)
    (config! users-frame :content users-panel)
    (listen users-frame :window-closing (partial close-users-frame users-menu))
    (config! users-menu :enabled? false)
    (pack! users-frame)
    (show! users-frame)))
