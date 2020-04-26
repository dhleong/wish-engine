(ns ^:no-doc wish-engine.api.limited-use
  (:require [wish-engine.util :refer [throw-msg]]))


; ======= validation ======================================

(defn validate-spec [s]
  (letfn [(throw-reason [& args]
            (throw-msg "Invalid limited-use ("
                       (apply str args)
                       "):\n"
                       s))]
    (when-not (:id s)
      (throw-reason "missing :id"))

    (when-let [amount (:restore-amount s)]
      (when-not (or (number? amount)
                    (ifn? amount))
        (throw-reason ":restore-amount must be a number or a fn")))

    ; return if valid
    s))


; ======= Compilation =====================================

(defn compile-spec [s]
  (as-> s s

    ; ensure :restore-amount is always a fn
    (if (ifn? (:restore-amount s)) s
      ;; if not provided, restore all
      (update s :restore-amount (fn [original]
                                  (cond
                                    ;; constant restore amount
                                    (number? original)
                                    (constantly original)

                                    ;; restore all if not otherwise specified
                                    (nil? original)
                                    (fn restore-all [{:keys [used]}]
                                      used)))))))

