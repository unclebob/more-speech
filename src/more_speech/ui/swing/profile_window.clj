(ns more-speech.ui.swing.profile-window
  (:require
    [more-speech.bech32 :as bech32]
    [more-speech.mem :refer :all]
    [more-speech.nostr.util :as util])
  (:use (seesaw [core])))

(defn close-profile-frame [menu _e]
  (config! menu :enabled? true))

(defn make-data-panel [field-name content id editable?]
  (let [the-label (label :text field-name :size [150 :by 20])
        the-field (text :text content
                        :editable? editable?
                        :id id
                        :size [800 :by 20])]
    (left-right-split the-label the-field)))

(defn make-profile-frame [_e]
  (let [profile-frame (frame :title "Profile")
        name-panel (make-data-panel "Name: "
                                    (get-mem [:keys :name])
                                    :name-field true)
        about-panel (make-data-panel "About:"
                                     (get-mem [:keys :about])
                                     :about-field true)
        picture-panel (make-data-panel "Picture:"
                                       (get-mem [:keys :picture])
                                       :picture-field true)
        pubkey-panel (make-data-panel "Public key (hex):"
                                      (get-mem [:keys :public-key])
                                      :pubkey-field false)
        npub-panel (make-data-panel "Public key (npub)"
                                    (bech32/encode "npub" (util/unhexify (get-mem [:keys :public-key])))
                                    :npub-field false)
        nip05-panel (make-data-panel "NIP05 Internet Id:"
                                     (get-mem [:keys :nip05])
                                     :nip05-field
                                     true)
        lud16-panel (make-data-panel "LUD16 Zap Address:"
                                     (get-mem [:keys :lud16])
                                     :lud16-field
                                     true)

        profile-panel (vertical-panel :items [name-panel
                                              about-panel
                                              picture-panel
                                              pubkey-panel
                                              npub-panel
                                              nip05-panel
                                              lud16-panel])
        profile-menu (select (get-mem :frame) [:#profile-menu])]
    (config! profile-frame :content profile-panel)
    (config! profile-menu :enabled? false)
    (listen profile-frame :window-closing (partial close-profile-frame profile-menu))
    (pack! profile-frame)
    (show! profile-frame)))
