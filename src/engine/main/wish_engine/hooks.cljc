(ns wish-engine.hooks
  (:require [wish-engine.state :as state]))

(defn run [engine-state hook-id entity]
  (if-let [hook (get-in (state/value engine-state)
                        [:wish-engine/config :hooks hook-id])]
    (hook entity)
    entity))
