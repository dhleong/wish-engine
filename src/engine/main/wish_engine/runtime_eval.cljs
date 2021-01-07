(ns wish-engine.runtime-eval
  (:require [clojure.string :as str]
            [wish-engine.runtime.config :as config]
            [wish-engine.runtime.destructure :refer [destructure-bindings]]
            [wish-engine.scripting-api :as api])
  (:require-macros [wish-engine.runtime.js :refer [export-fn export-sym]]))



; ======= Util fns ========================================

(defn- ->number [to-coerce]
  (when to-coerce
    (cond
      (number? to-coerce) to-coerce
      (not= -1 (.indexOf to-coerce ".")) (js/parseFloat to-coerce)
      :else (js/parseInt to-coerce))))

(defn ^{:export true :no-doc true} mathify [args]
  (map ->number args))

(defn ^{:export true :no-doc true} combinations
  "Produce a lazy sequence of all combinations of elements of `seqs`,
   as eg: `(for [s1 (nth seqs 0), s2 (nth seqs 1)] [s1 s2])`"
  [seqs]
  ; this is based on https://github.com/clojure/math.combinatorics
  ; (cartesian product)
  (let [v-original-seqs (vec seqs)
        step
        (fn step [v-seqs]
          (let [increment
                (fn increment [v-seqs]
                  (loop [i (dec (count v-seqs))
                         v-seqs v-seqs]
                    (if (= i -1)
                      nil
                      (if-let [rst (next (v-seqs i))]
                        (assoc v-seqs i rst)
                        (recur (dec i)
                               (assoc v-seqs i (v-original-seqs i)))))))]
            (when v-seqs
              (cons (mapv first v-seqs)
                    (lazy-seq (step (increment v-seqs)))))))]
    (when (every? seq seqs)
      (lazy-seq (step v-original-seqs)))))

(defn- wrap-exc [fn-name form-str e]
  (if (str/includes? (ex-message e) "of undefined")
    (ex-info (str "Unable to call unknown fn: " fn-name)
             {:unknown-fn fn-name
              :original-form form-str}
             e)
    e))

(defn ^{:export true :no-doc true} try-unsafe [fn-name form-str wrapped-fn]
  (try
    (wrapped-fn)
    (catch :default e
      (throw (wrap-exc fn-name form-str e)))))


; ======= Fn/macro export =================================

; start with these, which alias directly to JS functions
(def ^:no-doc exported-fns (merge {'ceil 'js/Math.ceil
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
(export-fn list)
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
(export-fn update)
(export-fn keys)
(export-fn vals)

(export-fn get)
(export-fn get-in)
(export-fn first)
(export-fn next)
(export-fn nth)

(export-fn comp)
(export-fn doall)
(export-fn filter)
(export-fn keep)
(export-fn map)
(export-fn mapcat)
(export-fn range)
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

(defn- process-for [seq-exprs body-expr]
  ; NOTE this is a *very* cut-down version, to work around limitations
  ; like not having lazy-seq or chunked fns exported; future work could add
  ; support for things like :when and :let
  (let [[seq-decls _filters] (split-with #(not (keyword? %)) seq-exprs)
        [seq-names seq-values] (->> seq-decls
                                    (partition 2)
                                    (apply map list))
        let-form (->> seq-names
                      (map-indexed list)
                      (mapcat (fn [[i n]]
                                [n (list (config/exported-fqn 'nth)
                                         'args-list
                                         i)]))
                      vec)]
    `(~(config/exported-fqn 'map)
                            (fn* [~'args-list]
                                 (let* ~let-form
                                   ~body-expr))
                            (combinations ~(vec seq-values)))))

(defn- process-doseq [seq-exprs body-expr]
  `(~(config/exported-fqn 'doall) ~(process-for seq-exprs body-expr)))

(defn- process-fn
  [bindings & body]
  (let [{bindings :bindings let-vector :let} (destructure-bindings bindings)]
    (if let-vector
      `(fn* ~bindings
            (let* ~let-vector
              ~@body))
      `(fn* ~bindings ~@body))))

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
(def ^:no-doc exported-macros
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

   'doseq process-doseq
   'for process-for

   'fn process-fn

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

(defn ^{:export true :no-doc true} vec-from-array [xs ^boolean no-clone]
  (cljs.core/PersistentVector.fromArray xs no-clone))

(defn ^{:export true :no-doc true} map-from-array [arr ^boolean no-clone]
  (cljs.core/PersistentHashMap.fromArray arr no-clone))

(defn ^{:export true :no-doc true} map-from-arrays [ks vs]
  (cljs.core/PersistentHashMap.fromArrays ks vs))
