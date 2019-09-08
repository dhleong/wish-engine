(ns wish-engine.util
  (:require [wish-engine.core :refer [create-engine]]
            [wish-engine.model :as engine]))

(defn eval-form [form]
  (let [eng (create-engine)
        state (engine/create-state eng)]
    (engine/eval-source-form eng state form)))

(defn eval-state [form]
  (let [eng (create-engine)
        state (engine/create-state eng)]
    (engine/eval-source-form eng state form)
    @state))


