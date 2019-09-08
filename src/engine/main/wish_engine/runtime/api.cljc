(ns wish-engine.runtime.api
  (:require [wish-engine.runtime.js :refer [export-fn-symbol-stmt]]))

(defmacro defn-api [name & body]
  `(do
     (defn ^:export ~name
       ~@body)

     ~(export-fn-symbol-stmt name name)))
