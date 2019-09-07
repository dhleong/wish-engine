(ns wish-engine.runtime-eval
  (:require [wish-engine.scripting-api :as api])
  (:require-macros [wish-engine.runtime.js :refer [export-fn export-sym]]))


; ======= Public API ======================================

;; explicitly export for cljs use:

(def ^:export has? api/has?)
(def ^:export ordinal api/ordinal)


; ======= Fn/macro export =================================

(defn- ->number [to-coerce]
  (when to-coerce
    (cond
      (number? to-coerce) to-coerce
      (not= -1 (.indexOf to-coerce ".")) (js/parseFloat to-coerce)
      :else (js/parseInt to-coerce))))

(defn ^:export mathify [args]
  (map ->number args))

(def exported-fns
  (-> { ; these alias directly to JS functions
       'ceil 'js/Math.ceil
       'floor 'js/Math.floor}

      ;;
      ;; Expose!
      ;;

      ;; (export-fn inc)
      ;; (export-fn dec)

      ;; (export-fn keyword)
      ;; (export-fn namespace)
      ;; (export-fn name)
      ;; (export-fn str)
      ;; (export-fn symbol)
      ;; (export-fn vector)

      ;; (export-fn concat)
      ;; (export-fn cons)
      ;; (export-fn contains?)
      ;; (export-fn count)
      ;; (export-fn identity)
      ;; (export-fn keys)
      ;; (export-fn vals)
      ;; (export-fn vec)

      ;; (export-fn get)
      ;; (export-fn get-in)

      ;; (export-fn comp)
      ;; (export-fn filter)
      ;; (export-fn keep)
      ;; (export-fn map)
      ;; (export-fn mapcat)
      ;; (export-fn remove)
      ;; (export-fn some)

      ;; (export-fn partial)

      ; for debugging
      ;; (export-fn println)
      ))

(export-fn + mathify)
(export-fn - mathify)
(export-fn / mathify)
(export-fn * mathify)
(export-fn <)
(export-fn >)
(export-fn <=)
(export-fn >=)
(export-fn =)
(export-fn not=)
(export-fn not)
(export-fn min)
(export-fn max)


(when-not js/goog.DEBUG
  ;; (export-macro ->)
  ;; (export-macro ->>)
  ;; (export-macro as->)
  ;; (export-macro cond)
  ;; (export-macro cond->)
  ;; (export-macro cond->>)
  ;; (export-macro if-let)
  ;; (export-macro if-not)
  ;; (export-macro if-some)
  ;; (export-macro some->)
  ;; (export-macro some->>)
  ;; (export-macro when)
  ;; (export-macro when-first)
  ;; (export-macro when-let)
  ;; (export-macro when-not)
  ;; (export-macro when-some)

  ; this is required for (cond)
  (export-sym cljs.core/truth_)

  (export-sym cljs.core/Symbol)
  (export-sym cljs.core/Keyword)
  (export-sym cljs.core/PersistentArrayMap)
  (export-sym cljs.core/PersistentHashMap)
  (export-sym cljs.core/PersistentHashSet)
  (export-sym cljs.core/PersistentVector))
