(ns wish-engine.core
  (:require [wish-engine.runtime :as runtime]))

(defn create-engine []
  (println "Hi!")
  {})

(defn ^:export eval-string [s]
  (let [eng (runtime/create-engine)]
    (runtime/eval-form eng (runtime/read-string s))))
