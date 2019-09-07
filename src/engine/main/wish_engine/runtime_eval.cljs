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
(export-fn inc)
(export-fn dec)

(export-fn nil?)
(export-fn keyword)
(export-fn namespace)
(export-fn name)
(export-fn str)
(export-fn symbol)
(export-fn vector)

(export-fn concat)
(export-fn cons)
(export-fn contains?)
(export-fn count)
(export-fn identity)
(export-fn keys)
(export-fn vals)
(export-fn vec)

(export-fn get)
(export-fn get-in)

(export-fn comp)
(export-fn filter)
(export-fn keep)
(export-fn map)
(export-fn mapcat)
(export-fn remove)
(export-fn some)

(export-fn partial)

; for debugging
(export-fn println)


(when-not js/goog.DEBUG
  ; this is required for (cond, etc.)
  (export-sym cljs.core/truth_)

  (export-sym cljs.core/Symbol)
  (export-sym cljs.core/Keyword)
  (export-sym cljs.core/PersistentArrayMap)
  (export-sym cljs.core/PersistentHashMap)
  (export-sym cljs.core/PersistentHashSet)
  (export-sym cljs.core/PersistentVector))

(defn- process-if-let
  ([bindings then] (process-if-let bindings then nil))
  ([bindings then else]
   (let [form (bindings 0)
         value (bindings 1)]
     `(let* [tmp# ~value]
        (if tmp#
          (let* [~form tmp#]
            ~then)
          ~else)))))

(defn- process-if-some
  ([bindings then] (process-if-some bindings then nil))
  ([bindings then else]
   (let [form (bindings 0)
         value (bindings 1)]
     `(let* [tmp# ~value]
        (if (wish-engine.runtime-eval/exported-nil? tmp#)
          ~else
          (let* [~form tmp#]
            ~then))))))

; most of this is borrowed from clojure.core
(def exported-macros
  {'-> (fn [x & forms]
         (loop [x x, forms forms]
           (if forms
             (let [form (first forms)
                   threaded (if (seq? form)
                              `(~(first form) ~x ~@(next form))
                              (list form x))]
               (recur threaded (next forms)))
             x)))

   '->> (fn [x & forms]
          (loop [x x, forms forms]
            (if forms
              (let [form (first forms)
                    threaded (if (seq? form)
                               `(~(first form) ~@(next form) ~x)
                               (list form x))]
                (recur threaded (next forms)))
              x)))

   'as-> (fn [expr n & forms]
           `(let* [~n ~expr
                   ~@(interleave (repeat n) (butlast forms))]
              ~(if (empty? forms)
                 n
                 (last forms))))

   'cond (fn process-cond [& clauses]
           (when clauses
             (list 'if (first clauses)
                   (if (next clauses)
                     (second clauses)
                     (throw (js/Error.
                              "cond requires an even number of forms")))
                   (apply process-cond (nnext clauses)))))

   'if-let process-if-let

   'if-not (fn [condition & body]
             `(if (not ~condition)
                ~@body))

   'if-some process-if-some

   ;; (export-macro some->)
   ;; (export-macro some->>)

   'when (fn [condition & body]
           `(if ~condition (do ~@body)))

   'when-let (fn [bindings & body]
               (process-if-let bindings `(do ~@body)))

   ;; (export-macro when-first)
   ;; (export-macro when-not)

   'when-some (fn [bindings & body]
                (process-if-some bindings `(do ~@body)))

   })

