(ns wish-engine.ci
  (:require [wish-engine.core :refer [create-engine]]
            [wish-engine.model :as engine]))

(defn ^:export eval-string
  ([^String s] (eval-string {} s))
  ([state, ^String s]
   (let [engine (create-engine)
         state (or state (engine/create-state engine))
         form (engine/parse-string engine s)]
     (engine/eval-source-form engine state form))))
