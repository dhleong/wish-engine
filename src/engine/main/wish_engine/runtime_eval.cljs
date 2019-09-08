(ns wish-engine.runtime-eval
  (:require [wish-engine.runtime.config :as config]
            [wish-engine.scripting-api :as api])
  (:require-macros [wish-engine.runtime.js :refer [export-fn export-sym]]))


; ======= Fn/macro export =================================

(defn- ->number [to-coerce]
  (when to-coerce
    (cond
      (number? to-coerce) to-coerce
      (not= -1 (.indexOf to-coerce ".")) (js/parseFloat to-coerce)
      :else (js/parseInt to-coerce))))

(defn ^:export mathify [args]
  (map ->number args))

; start with these, which alias directly to JS functions
(def exported-fns (merge {'ceil 'js/Math.ceil
                          'floor 'js/Math.floor}
                         api/exported-fns))

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

(export-fn identity)
(export-fn keyword)
(export-fn namespace)
(export-fn name)
(export-fn nil?)
(export-fn str)
(export-fn symbol)
(export-fn vector)

(export-fn concat)
(export-fn cons)
(export-fn contains?)
(export-fn count)
(export-fn vec)

(export-fn assoc)
(export-fn keys)
(export-fn vals)

(export-fn get)
(export-fn get-in)
(export-fn first)
(export-fn next)

(export-fn comp)
(export-fn filter)
(export-fn keep)
(export-fn map)
(export-fn mapcat)
(export-fn remove)
(export-fn some)
(export-fn seq)

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

(defn- process-and
  ([] true)
  ([x] x)
  ([x & others]
   `(let* [and# ~x]
      (if and#
        ~(apply process-and others)
        and#))))

(defn- process-or
  ([] nil)
  ([x] x)
  ([x & others]
   `(let* [or# ~x]
      (if or#
        or#
        ~(apply process-or others)))))

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
        (if (~(config/exported-fqn 'nil?) tmp#)
          ~else
          (let* [~form tmp#]
            ~then))))))

(defn- thread-with [threader]
  (fn thread-with-fn [x & forms]
    (loop [x x, forms forms]
      (if forms
        (let [form (first forms)
              threaded (if (seq? form)
                         (threader x form)
                         (list form x))]
          (recur threaded (next forms)))
        x))))

(def ^:private thread-> (thread-with
                          (fn [x form]
                            `(~(first form) ~x ~@(next form)))))

(def ^:private thread->> (thread-with
                           (fn [x form]
                             `(~(first form) ~@(next form) ~x))))

(defn- thread-some-with [threader]
  (fn thread-some-fn [expr & forms]
    (let [g (gensym)
          steps (map (fn [step] `(if (~(config/exported-fqn 'nil?) ~g)
                                   nil
                                   ~(threader g step)))
                     forms)]
      `(let* [~g ~expr
              ~@(interleave (repeat g) (butlast steps))]
         ~(if (empty? steps)
            g
            (last steps))))))

; most of this is borrowed from clojure.core
(def exported-macros
  {'-> thread->
   '->> thread->>

   'as-> (fn [expr n & forms]
           `(let* [~n ~expr
                   ~@(interleave (repeat n) (butlast forms))]
              ~(if (empty? forms)
                 n
                 (last forms))))

   'and process-and
   'or process-or

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
             `(if (~(config/exported-fqn 'not) ~condition)
                ~@body))

   'if-some process-if-some

   'some-> (thread-some-with thread->)
   'some->> (thread-some-with thread->>)

   'when (fn [condition & body]
           `(if ~condition (do ~@body)))

   'when-let (fn [bindings & body]
               (process-if-let bindings `(do ~@body)))

   'when-first (fn [bindings & body]
                 (let [[x xs] bindings
                       xs-var (gensym "xs")]
                   (process-if-let [xs-var (list (config/exported-fqn 'seq)
                                                 xs)]
                                   `(let* [~x (~(config/exported-fqn 'first) ~xs-var)]
                                      ~@body))))

   'when-not (fn [condition & body]
               `(if (~(config/exported-fqn 'not) ~condition)
                  (do ~@body)))

   'when-some (fn [bindings & body]
                (process-if-some bindings `(do ~@body)))


   ; ======= wish-engine-specific macros =====================

   'on-state (fn [& body]
               `(fn* [~'state]
                  ~(apply thread-> 'state body)))

   })

