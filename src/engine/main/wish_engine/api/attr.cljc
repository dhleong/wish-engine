(ns ^:no-doc wish-engine.api.attr
  (:require [wish-engine.runtime.state :refer [*apply-context*]]
            [wish-engine.util :as util :refer [throw-arg]]))

(defn provide [state attr-id-or-path value]
  (let [attr-path (cond
                    (vector? attr-id-or-path) attr-id-or-path
                    (keyword? attr-id-or-path) [attr-id-or-path]
                    :else (throw-arg "provide-attr" attr-id-or-path
                                     ":attr or [:attr :path]"))

        state (assoc-in state (cons :attrs attr-path) value)]

    (if-let [context *apply-context*]
      (assoc-in state
                (cons :attrs/meta attr-path)
                {:wish-engine/source context})
      state)))
