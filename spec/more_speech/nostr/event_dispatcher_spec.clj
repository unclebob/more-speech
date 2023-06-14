(ns more-speech.nostr.event-dispatcher-spec
  (:require
    [more-speech.db.gateway :as gateway]
    [more-speech.mem :refer :all]
    [more-speech.nostr.event-dispatcher :as ed]
    [more-speech.nostr.events :as events]
    [more-speech.spec-util :refer :all]
    [speclj.core :refer :all]))

(declare db)
(describe "event-dispatcher"
  (with-stubs)
  (setup-db-mem)

  (context "cross references"
    (it "cross references an existing event."
      (with-redefs
        [events/get-references (stub :get-references {:return [nil nil 1]})
         gateway/event-exists? (stub :event-exists? {:return true})
         gateway/add-reference-to-event (stub :add-reference)]
        (ed/add-cross-reference @db {:id 2})
        (should-have-invoked :add-reference {:with [:* 1 2]})))

    (it "creates an orphaned reference if referenced event does not exist"
      (with-redefs
        [events/get-references (stub :get-references {:return [nil nil 1]})
         gateway/event-exists? (stub :event-exists? {:return false})
         gateway/add-reference-to-event (stub :add-reference)]
        (ed/add-cross-reference @db {:id 2})
        (should-not-have-invoked :add-reference)
        (should= {1 [2]} (get-mem [:orphaned-replies]))))

    (it "adds orphaned replies to parent."
      (set-mem :orphaned-replies {1 [2 3]})
      (with-redefs
        [events/get-references (stub :get-references {:return [nil nil nil]})
         gateway/event-exists? (stub :event-exists?)
         gateway/add-reference-to-event (stub :add-reference)]
        (ed/add-cross-reference @db {:id 1})
        (should-have-invoked :add-reference {:with [:* 1 2]})
        (should-have-invoked :add-reference {:with [:* 1 3]})
        (should= nil (get-mem [:orphaned-replies 1]))
        )
      )
    )
  )