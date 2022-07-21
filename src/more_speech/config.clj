(ns more-speech.config)

(def default-font "COURIER-PLAIN-14")
(def bold-font "COURIER-BOLD-14")
(def small-font "COURIER_PLAIN-9")

(def article-width 120)

(def days-to-read 10) ;how many daily message files to read in.

(def migration-level 7)

(def test-run? false)
;---configuration files
(def private-directory (atom "private"))
(def migration-filename (atom "private/migration"))
(def nicknames-filename (atom "private/nicknames")) ;grandfathered.
(def profiles-filename (atom "private/profiles"))
(def keys-filename (atom "private/keys"))
(def relays-filename (atom "private/relays"))
(def read-event-ids-filename (atom "private/read-event-ids"))
(def tabs-filename (atom "private/tabs")) ;grandfathered.
(def tabs-list-filename (atom "private/tabs-list"))
(def messages-directory (atom "private/messages"))
(def messages-filename (atom "private/messages/message-file"))

(def user-name-pattern #"\@[\w\-]+")
(def user-name-chars #"[\w\-]+")
(def user-name-max-length 15)
