(ns more-speech.mem-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.swing.ui-context :refer :all]))

(describe "memory operators"
  (before (clear-mem))

  (it "unknown values are nil"
    (should-be-nil (get-mem :unknown)))

  (it "gets a value that was set"
    (set-mem :name :value)
    (should= :value (get-mem :name)))

  (it "sets and gets a nested value"
    (set-mem [:name :sub-name] :value)
    (should= :value (get-mem [:name :sub-name])))

  (it "updates a value"
    (set-mem :list [])
    (update-mem :list conj 1)
    (should= [1] (get-mem :list)))

  (it "updates a nested value"
    (set-mem [:name :list] [])
    (update-mem [:name :list] conj 1)
    (should= [1] (get-mem [:name :list])))
  )
