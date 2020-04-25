(ns ^:no-doc wish-engine.runtime.destructure
  (:require [wish-engine.runtime.config :as config]
            [wish-engine.util :refer [throw-msg]]))

(defn- destructure-from-map [symbols]
  (let [arg-sym (gensym "map__")
        get-sym (config/exported-fqn 'get)]
    {:bindings [arg-sym]
     :let (->> symbols
               (mapcat (fn [sym]
                         [sym `(~get-sym
                                 ~arg-sym
                                 ~(keyword (name sym)))]))
               vec)}))

(defn- destructure-from-vector [symbols]
  (let [arg-sym (gensym "vec__")
        nth-sym (config/exported-fqn 'nth)]
    {:bindings [arg-sym]
     :let (->> symbols
               (map-indexed (fn [index sym]
                              [sym `(~nth-sym
                                      ~arg-sym
                                      ~index
                                      nil)]))
               (apply concat)
               vec)}))

(defn destructure-bindings
  "Given a vector of bindings, return a map of :bindings and :let"
  [bindings]
  (when-not (vector? bindings)
    (throw-msg "Bindings to fn must be vector; got: " bindings))

  (let [first-item (first bindings)]
    (cond
      (every? symbol? bindings)
      {:bindings bindings}

      (and (map? first-item)
           (contains? first-item :keys))
      (destructure-from-map (:keys first-item))

      (set? first-item)
      (destructure-from-map first-item)

      (vector? first-item)
      (destructure-from-vector first-item)

      :else
      (throw-msg "Unexpected destructuring form: " bindings))))
