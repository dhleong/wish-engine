(ns wish-engine.core
  (:require [wish-engine.runtime :as runtime]))

(defn create-engine []
  (runtime/create-engine))
