(ns more-speech.types.user-window
  (:require [clojure.spec.alpha :as s]))

(s/def ::id number?)
(s/def ::ids (s/coll-of ::id))

(s/def ::user-name string?)

(s/def ::item (s/tuple ::user-name ::id))
(s/def ::items (s/coll-of ::item))

(s/def ::contact-list-changed boolean?)
;;;;;;;;;;true if the new contact list should be sent to the relays.

(s/def ::trusted-users ::ids)
(s/def ::recent-users ::ids)
(s/def ::web-of-trust-users ::ids)
(s/def ::trusted-user-items ::items)
(s/def ::recent-user-items ::items)
(s/def ::web-of-trust-items ::items)
(s/def ::selection-group #{:recent-user-items
                           :trusted-user-items
                           :web-of-trust-items})

(s/def ::user-window
  (s/or :nil nil?
        :user-window-active (s/keys
                              :opt-un [::contact-list-changed
                                       ::trusted-users
                                       ::trusted-user-items
                                       ::recent-users
                                       ::recent-user-items
                                       ::web-of-trust-users
                                       ::web-of-trust-items
                                       ::selection-group])))
