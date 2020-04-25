(ns ^:no-doc wish-engine.runtime.api
  (:require [wish-engine.runtime.js :refer [export-fn-symbol-stmt
                                            set-assoc-stmt]]))

(defmacro defn-api [fn-name & body]
  `(do
     (defn ^:export ~fn-name
       ~@body)

     ~(set-assoc-stmt 'exported-fn-refs
                      `(symbol ~(name fn-name))
                      fn-name)
     ~(export-fn-symbol-stmt fn-name fn-name)))
