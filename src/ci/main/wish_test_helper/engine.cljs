(ns wish-test-helper.engine
  "This namespace needs to be distinct so it doesn't get overwritten
   by the wish_engine definition from the karma test file"
  (:require [wish-engine.core :as core]))

(defn ^:export eval-string
  ([^String s] (eval-string {} s))
  ([state, ^String s]
   (let [engine (core/create-engine)
         state (or state (core/create-state engine))]
     (core/load-source engine state s))))
