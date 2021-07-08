(ns wish-engine.spec.class
  (:require [clojure.spec.alpha :as s]))

(s/def ::id keyword?)
(s/def ::identified (s/keys :req-un [::id]))

(s/def ::name string?)
(s/def ::named (s/keys :req-un [::named]))

(s/def ::! ifn?)
(s/def ::state-modifier (s/keys :req-un [::!]))
(s/def ::state-modifiable (s/keys :opt-un [::!]))

(s/def ::base-entity (s/and ::identified
                            ::named
                            ::state-modifiable))

(s/def ::levels (s/map-of number? ::state-modifier))
(s/def ::levelable (s/keys :opt-un [::levels]))

(s/def ::obj (s/and ::base-entity
                    ::levelable))

