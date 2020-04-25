(ns ^:no-doc wish-engine.runtime
  "Dummy engine implementation for JVM"
  (:require [clojure.edn :as edn]
            [wish-engine.edn :refer [edn-readers]]
            [wish-engine.model :refer [WishEngine]]
            [wish-engine.runtime.state :refer [*engine-state*]]
            [wish-engine.state :as state]))

(defn- eval-form [engine form]
  (println "TODO: eval" engine form))

(deftype JVMWishEngine [config]
  WishEngine
  (create-state [this] (state/create {:wish-engine/config config}))
  (parse-string [this s]
    (edn/read-string {:readers edn-readers} s))
  (eval-source-form [this state form]
    (binding [*engine-state* state]
      (eval-form this form))))

(defn create-engine [config]
  (->JVMWishEngine config))
