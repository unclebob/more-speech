(ns more-speech.config)

(def default-font "COURIER-PLAIN-14")
(def bold-font "COURIER-BOLD-14")
(def small-font "COURIER_PLAIN-9")

(def article-width 120)

(def subscribe-days-ago 4) ;get n days worth of events from the relays.

(def migration-level 2)
;---configuration files
(def private-directory (atom "private"))
(def migration-filename (atom "private/migration"))
(def nicknames-filename (atom "private/nicknames"))
(def keys-filename (atom "private/keys"))
(def relays-filename (atom "private/relays"))
(def read-event-ids-filename (atom "private/read-event-ids"))
(def tabs-filename (atom "private/tabs"))

(def user-name-pattern #"\@[\w\-]+")
(def user-name-chars #"[\w\-]+")
(def user-name-max-length 15)
