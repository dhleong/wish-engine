(ns wish-engine.runtime-eval
  (:require [wish-engine.scripting-api :as api])
  (:require-macros [wish-engine.runtime.js :refer [expose-fn export-macro export-sym]]))


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

(def exposed-fns
  (-> { ; these alias directly to JS functions
       'ceil 'js/Math.ceil
       'floor 'js/Math.floor}

      ;;
      ;; Expose!
      ;;

      (expose-fn + mathify)
      (expose-fn - mathify)
      (expose-fn / mathify)
      (expose-fn * mathify)
      (expose-fn <)
      (expose-fn >)
      (expose-fn <=)
      (expose-fn >=)
      (expose-fn =)
      (expose-fn not=)
      (expose-fn not)
      (expose-fn min)
      (expose-fn max)

      (expose-fn inc)
      (expose-fn dec)

      (expose-fn keyword)
      (expose-fn namespace)
      (expose-fn name)
      (expose-fn str)
      (expose-fn symbol)
      (expose-fn vector)

      (expose-fn concat)
      (expose-fn cons)
      (expose-fn contains?)
      (expose-fn count)
      (expose-fn identity)
      (expose-fn keys)
      (expose-fn vals)
      (expose-fn vec)

      (expose-fn get)
      (expose-fn get-in)

      (expose-fn comp)
      (expose-fn filter)
      (expose-fn keep)
      (expose-fn map)
      (expose-fn mapcat)
      (expose-fn remove)
      (expose-fn some)

      (expose-fn partial)

      ; for debugging
      (expose-fn println)))

(when-not js/goog.DEBUG
  (export-macro ->)
  (export-macro ->>)
  (export-macro as->)
  (export-macro cond)
  (export-macro cond->)
  (export-macro cond->>)
  (export-macro if-let)
  (export-macro if-not)
  (export-macro if-some)
  (export-macro some->)
  (export-macro some->>)
  (export-macro when)
  (export-macro when-first)
  (export-macro when-let)
  (export-macro when-not)
  (export-macro when-some)

  ; this is required for (cond)
  (export-sym cljs.core/truth_)

  (export-sym cljs.core/Symbol)
  (export-sym cljs.core/Keyword)
  (export-sym cljs.core/PersistentArrayMap)
  (export-sym cljs.core/PersistentHashMap)
  (export-sym cljs.core/PersistentHashSet)
  (export-sym cljs.core/PersistentVector))
