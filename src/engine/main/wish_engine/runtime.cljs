(ns wish-engine.runtime
  (:require [clojure.string :as str]
            [clojure.analyzer.api :refer-macros [no-warn]]
            [clojure.walk :refer [prewalk]]
            [cljs.reader :as edn]
            [cljs.js :as cljs :refer [empty-state js-eval]]
            [com.rpl.specter :as sp]
            [wish-engine.edn :refer [edn-readers]]
            [wish-engine.model :refer [WishEngine]]
            [wish-engine.runtime.config :as config]
            [wish-engine.runtime.selectors :as selectors]
            [wish-engine.runtime.state :refer [*engine-state*]]
            [wish-engine.runtime-eval :refer [exported-fns exported-macros]]
            [wish-engine.scripting-api :as api]
            [wish-engine.state :as state]
            [wish-engine.util :refer [form?]]))


; ======= Consts ==========================================

(def ^:private nil-symbol (symbol "nil"))
(def ^:private false-symbol (symbol "false"))


; ======= export fns/vars/macros for use ==================

;;; code rewriting for compilation

(defn- ->kw-get
  "Under advanced compilation, the function names to invoke
   a keyword as a function have been munged and are unavailable.
   We could force people to use (get), but it's nicer to just
   rewrite it that way ourselves."
  [kw m & args]
  (concat (list (config/exported-fqn 'get) m kw)
          args))

(defn- ->has?  [args]
  (cons (config/with-exported-ns 'has?)
        args))

(defn- ->special-form [sym]
  (get {'let 'let*} sym))

(defn- unknown-fn-call? [fn-call]
  (and (nil? (namespace fn-call))
       (not (contains? exported-fns fn-call))
       (not (#{'fn* 'let* 'do 'try 'if 'quote} fn-call))))

(defn- ->compilable-symbol [sym]
  (or (get exported-fns sym)
      (->special-form sym)

      (when (= "js" (namespace sym))
        nil-symbol)))

(defn- ->compilable-list [[fn-call :as sym]]
  (or (when (symbol? fn-call)
        (when-let [macro (get exported-macros (first sym))]
          (let [evaluated (apply macro (rest sym))]
            (case evaluated
              nil nil-symbol
              false false-symbol
              evaluated))))

      (cond
        (keyword? fn-call)
        (apply ->kw-get sym)

        ; easy case; fall through to return unchanged
        (not (symbol? fn-call))
        nil

        (and (= 'wish-engine.runtime/exported-some
                fn-call)
             (set? (second sym)))
        (->has? (rest sym))

        (unknown-fn-call? fn-call)
        `(~(config/with-exported-ns 'try-unsafe)
           ~(str fn-call)
           ~(str sym)
           (fn* [] ~sym)))))

(defn- ->compilable
  "Given a raw symbol/expr, return something that we can actually compile."
  [sym]
  (let [result (cond
                 (symbol? sym) (->compilable-symbol sym)
                 (form? sym) (->compilable-list sym))]
    (condp identical? result
      nil sym  ; return unchanged
      nil-symbol nil
      false-symbol false
      result)))

(defn- process-source [js-src]
  (-> js-src
      (str/replace
        ; we could also just replace the _invoke sequence, but
        ; that may or may not be safe....
        #"(new cljs\.core\.Keyword\(null,\"[^\"]+\",\"[^\"]+\",\([0-9-]+\)\))\.cljs\$core\$IFn\$_invoke\$arity\$([0-9]+)\("
        "$1.call(null,")
      (str/replace
        #"cljs\.core\.PersistentVector\.fromArray"
        "wish_engine.runtime_eval.vec_from_array")
      (str/replace
        #"cljs\.core\.PersistentHashMap\.fromArray"
        "wish_engine.runtime_eval.map_from_array")
      (str/replace
        #"cljs\.core\.list"
        "wish_engine.runtime_eval.exported_list")))


; ======= Evaluation ======================================

(defn- eval-err [form src e]
  (str "FAILED to js/eval:\n\n"
       (:source src)
       "\n\nOriginal form: " form
       "\n\nOriginal error: " (.-stack e)))

(defn- eval-in [state form]
  (cljs/eval state
             form
             {:eval (fn [src]
                      (let [src (update src :source process-source)]
                        (try
                          (js-eval src)
                          (catch :default e
                            ; ex-info-based errors can be thrown directly,
                            ; and indicate parse errors, etc.
                            (if (ex-data e)
                              (throw e)

                              (let [msg (eval-err form src e)]
                                (js/console.warn msg)
                                (throw (js/Error. msg))))))))

              :context :expr
              :ns config/runtime-eval-ns}
             (fn [res]
               (if (contains? res :value) ; nil or false are fine
                 (:value res)
                 (when-not (= "Could not require wish-engine.runtime"
                              (ex-message (:error res)))
                   (throw (ex-info
                            (str "Error evaluating: " form "\n" res)
                            {}
                            (:error res))))))))

(defn- create-new-eval-state []
  (let [new-state (empty-state)]
    ; HACKS: this is some major hacks for compat with advanced compilation.
    ; Basically, in the analysis step, it occasionally gets to a point where
    ; it wants to check if the type of a clause is a clojure variable, and
    ; to do that it eventually attempts to resolve the symbol as a macro
    ; in the cljs.core namespace. The code assumes that the cljs.core$macros
    ; namespace exists, but under advanced compilation it does *not*, by
    ; default. So, we make it happen.
    (when-not (or js/goog.DEBUG
                  cljs.core.NS_CACHE)
      (set! cljs.core.NS_CACHE
            (atom {'cljs.core$macros (cljs.core.Namespace.
                                       #js {}
                                       "cljs.core$macros")})))

    ; eval an ns form so the imports are recognized
    (eval-in
      new-state
      (list
        'ns config/runtime-eval-ns
        '(:require [wish-engine.runtime])))

    new-state))

(defn clean-form
  "'Clean' the given form and all its subforms so that it can be
   successfully evaluated"
  [form]
  (sp/compiled-transform
    selectors/compilable-target-path
    ->compilable
    form))

(defn- eval-cleaned-form [engine form]
  (try
    (no-warn
      (eval-in @(.-eval-state engine) form))
    (catch :default e
      (js/console.error "Error compiling:" (str form) e)
      (when-let [cause (.-cause e)]
        (js/console.error "Cause: " (.-stack cause))
        (when-let [cause2 (.-cause cause)]
          (js/console.error "Cause2: " (.-stack cause2))
          (when-let [cause3 (.-cause cause2)]
            (js/console.error "Cause3: " (.-stack cause3)))))
      (throw e))))

(defn- needs-eval? [fn-call]
  (when fn-call
    (or (#{'fn* 'let*} fn-call)
        (#{"wish-engine.runtime-eval"
           "wish-engine.scripting-api"} (namespace fn-call)))))

(defn- eval-if-necessary [engine form]
  (let [fn-call (when (and (form? form)
                           (symbol? (first form)))
                  (first form))]
    (cond
      (= 'quote fn-call)
      (second form)

      ; lazily compile fns
      (= 'fn* fn-call)
      (let [lazy-fn (delay (eval-cleaned-form engine form))]
        (fn [& args]
          (apply @lazy-fn args)))

      (needs-eval? fn-call)
      (eval-cleaned-form engine form)

      :else
      form)))

(defn- eval-args-as-necessary [engine args]
  (prewalk (partial eval-if-necessary engine) args))

(defn- eager-evaluate [engine api-fn args]
  (let [evaluated-args (eval-args-as-necessary engine args)]
    (apply api-fn evaluated-args)))

(defn- eager-evaluatable-fn [form]
  (when (and (form? form)
             (symbol? (first form)))
    (let [fn-call (first form)]
      (get api/exported-fn-refs (symbol (name fn-call))))))

(defn eval-form [engine form]
  (let [cleaned (clean-form form)]
    (if-let [f (eager-evaluatable-fn cleaned)]
      (eager-evaluate engine f (rest cleaned))
      (eval-cleaned-form engine cleaned))))


; ======= Public interface ================================

(deftype JSWishEngine [eval-state config]
  WishEngine
  (create-state [this] (state/create {:wish-engine/config config}))
  (parse-string [this s]
    (edn/read-string {:readers edn-readers} s))
  (eval-source-form [this state form]
    (binding [*engine-state* state]
      (eval-form this form))))

(defn create-engine [config]
  (->JSWishEngine (delay (create-new-eval-state))
                  config))
