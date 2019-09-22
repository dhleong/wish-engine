(ns wish-engine.runtime.selectors
  (:require [com.rpl.specter :as sp]
            [wish-engine.util :refer [form?]]))

(def compilable-target
  (sp/recursive-path
    [] p
    (sp/cond-path
      symbol? sp/STAY
      map? [sp/MAP-VALS p]
      vector? [sp/ALL-WITH-META p]
      form? (sp/cond-path
              ; ignore (quote) forms completely
              (sp/pred (fn [v] (= 'quote (first v))))
              sp/STOP

              ; post-order traverse into the fn-call
              (sp/pred (fn [v] (or (symbol? (first v))
                                   (keyword? (first v)))))
              (sp/continue-then-stay [sp/ALL-WITH-META p])

              ; just keep looking
              :else [sp/ALL-WITH-META p])

      :else sp/STOP)))

(def compilable-target-path (sp/comp-paths compilable-target))
