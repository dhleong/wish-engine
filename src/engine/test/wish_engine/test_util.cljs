(ns wish-engine.test-util
  (:require [wish-engine.core :refer [create-engine]]
            [wish-engine.model :as engine]))

(defn eval-form [form]
  (let [eng (create-engine)
        state (engine/create-state eng)]
    (engine/eval-source-form eng state form)))

(defn eval-state [& forms]
  (let [eng (create-engine)
        state (engine/create-state eng)]
    (doseq [f forms]
      (engine/eval-source-form eng state f))
    @state))

