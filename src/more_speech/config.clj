(ns more-speech.config
  (:require [clojure.string :as string])
  (:import (dev.dirs ProjectDirectories))
  )

(def default-font "COURIER-PLAIN-14")
(def bold-font "COURIER-BOLD-14")
(def small-font "COURIER_PLAIN-9")

(def article-width 120)

(def days-to-read 10) ;how many daily message files to read in.

(def migration-level 8)
(def version "202207310819")

(def test-run? false)
;---configuration files
(def private-directory (atom (.-configDir (ProjectDirectories/from "org" "unclebob" "more-speech"))))
(def migration-filename (atom (string/join "/" [@private-directory "migration"])))
(def nicknames-filename (atom (string/join "/" [@private-directory "nicknames"]))) ;grandfathered.
(def profiles-filename (atom (string/join "/" [@private-directory, "profiles"])))
(def keys-filename (atom (string/join "/" [@private-directory, "keys"])))
(def relays-filename (atom (string/join "/" [@private-directory, "relays"])))
(def read-event-ids-filename (atom (string/join "/" [@private-directory, "read-event-ids"])))
(def tabs-filename (atom (string/join "/" [@private-directory, "tabs"]))) ;grandfathered.
(def tabs-list-filename (atom (string/join "/" [@private-directory, "tabs-list"])))
(def messages-directory (atom (string/join "/" [@private-directory, "messages"])))
(def messages-filename (atom (string/join "/" [@private-directory, "messages/message-file"])))
(def user-configuration-filename (atom (string/join "/" [@private-directory, "user-configuration"])))

(def user-name-pattern #"\@[\w\-]+")
(def user-name-chars #"[\w\-]+")
