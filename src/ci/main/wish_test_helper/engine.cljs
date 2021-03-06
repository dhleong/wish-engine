(ns wish-test-helper.engine
  "This namespace needs to be distinct so it doesn't get overwritten
   by the wish_engine definition from the karma test file"
  (:require [wish-engine.core :as core]
            [wish-engine.model :as m]))

(defn ^:export eval-string
  ([^String s] (eval-string nil s))
  ([state, ^String s]
   (let [engine (core/create-engine)
         state (or state (core/create-state engine))
         form (m/parse-string engine (str "(do " s ")"))]
     (m/eval-source-form engine state form))))

(defn ^:export load-string-source
  ([^String s] (load-string-source nil s))
  ([state, ^String s]
   (let [engine (core/create-engine)
         state (or state (core/create-state engine))]
     (core/load-source engine state s))))
