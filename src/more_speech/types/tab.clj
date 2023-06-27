(ns more-speech.types.tab
  (:require [clojure.spec.alpha :as s])
  (:import (javax.swing JTree)))

(s/def ::id number?)
(s/def ::name string?)

;The :selected list contains either ids, strings, or the :empty keyword.
;the latter represents a filter that matches nothing, whereas [] matches everything.
(s/def ::selected (s/coll-of
                    (s/or :id ::id
                          :pattern string?
                          :keyword (s/and keyword? #(= :empty %)))))

(s/def ::blocked (s/coll-of ::id))
(s/def ::tab (s/keys :req-un [::name ::selected ::blocked]))
(s/def ::tabs-list (s/coll-of ::tab))

(s/def ::tab-tree-map (s/map-of string? #(instance? JTree %)))

;The control structure for a search.  Contains the target regex,
;the index of the result being displayed, and the list of event ids matched.
(s/def ::search-results (s/tuple string? int? (s/coll-of ::id)))

;The search target for a given tab.
(s/def ::tab-search (s/map-of ::name ::search-results))