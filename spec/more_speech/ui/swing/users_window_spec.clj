(ns more-speech.ui.swing.users-window-spec
  (:require
    [more-speech.mem :refer :all]
    [more-speech.spec-util :refer :all]
    [more-speech.ui.swing.users-window :as users-window]
    [speclj.core :refer :all]))

(declare db)

(describe "users window"
  (with-stubs)
  (setup-db-mem)

  (context "selection management"
    (it "removes ids from selection lists"
      (set-mem [:user-window :some-items]
               [["id1" 1]
                ["id2" 2]
                ["id3" 3]])

      (set-mem [:user-window :some-ids]
               [1 2 3])
      (users-window/remove-item 2 :some-items :some-ids)
      (should= [["id1" 1]
                ["id3" 3]]
               (get-mem [:user-window :some-items]))
      (should= [1 3] (get-mem [:user-window :some-ids])))

    (it "finds item using pubkey"
      (should= ["id2" 2] (users-window/find-item 2 [["id1" 1]
                                                    ["id2" 2]
                                                    ["id3" 3]]))
      )
    )
  )



