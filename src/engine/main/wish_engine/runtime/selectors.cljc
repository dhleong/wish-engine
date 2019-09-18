(ns wish-engine.runtime.selectors
  (:require [com.rpl.specter :as sp]))

(def compilable-target
  (sp/recursive-path
    [] p
    (sp/cond-path
      symbol? sp/STAY
      map? [sp/MAP-VALS p]
      vector? [sp/ALL-WITH-META p]
      list? (sp/cond-path
              ; ignore (quote) forms completely
              (sp/pred (fn [v] (= 'quote (first v))))
              sp/STOP

              ; pre-order traverse into the fn-call
              (sp/pred (fn [v] (or (symbol? (first v))
                                   (keyword? (first v)))))
              (sp/stay-then-continue [sp/ALL-WITH-META p])

              ; just keep looking
              :else [sp/ALL-WITH-META p])

      :else sp/STOP)))

(def compilable-target-path (sp/comp-paths compilable-target))
