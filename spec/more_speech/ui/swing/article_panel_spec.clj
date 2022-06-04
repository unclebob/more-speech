(ns more-speech.ui.swing.article-panel-spec
  (:require [speclj.core :refer :all]
            [more-speech.ui.swing.article-panel :refer :all]
            [more-speech.ui.swing.ui-context :refer :all]
            [more-speech.nostr.util :as util]))

(defn hexify [n] (util/num32->hex-string n))

