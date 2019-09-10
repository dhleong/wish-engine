(ns wish-engine.runtime
  (:require [clojure.string :as str]
            [clojure.analyzer.api :refer-macros [no-warn]]
            [clojure.walk :refer [postwalk]]
            [cljs.reader :as edn]
            [cljs.js :as cljs :refer [empty-state js-eval]]
            [wish-engine.model :refer [WishEngine]]
            [wish-engine.runtime.config :as config]
            [wish-engine.runtime.state :refer [*engine-state*]]
            [wish-engine.runtime-eval :refer [exported-fns exported-macros]]))


; ======= Consts ==========================================

(def ^:private nil-symbol (symbol "nil"))



; ======= export fns/vars/macros for use ==================

;;; code rewriting for compilation

(declare ->compilable)

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
  (get
    {'let 'let*
     'fn 'fn*}
    sym))

(defn- ->compilable
  "Given a raw symbol/expr, return something that we
   can actually compile"
  [sym]
  (let [result (or (get exported-fns sym)
                   (->special-form sym)

                   (when (and (symbol? sym)
                              (= "js" (namespace sym)))
                     nil-symbol)

                   (when (and (list? sym) (symbol? (first sym)))
                     (when-let [macro (get exported-macros (first sym))]
                       (let [evaluated (apply macro (rest sym))]
                         (case evaluated
                           nil nil-symbol
                           false `(do false)
                           evaluated))))

                   (when (list? sym)
                     (let [fn-call (first sym)]
                       (if (keyword? fn-call)
                         (apply ->kw-get sym)

                         (or (condp = fn-call
                               'wish-engine.runtime/exported-some
                               (when (set? (second sym))
                                 (->has? (rest sym)))

                               ; else, fall through:
                               nil)

                             (when (and (nil? (namespace fn-call))
                                        (not (contains? exported-fns fn-call))
                                        (not (#{'fn* 'do 'try} fn-call)))
                               `(~(config/with-exported-ns 'try-unsafe)
                                  ~(str fn-call)
                                  ~(str sym)
                                  (fn* [] ~sym)))))))

                   ; just return unchanged
                   sym)]
    (when-not (identical? nil-symbol result)
      result)))

(defn- process-source [js-src]
  (-> js-src
      (str/replace
        ; we could also just replace the _invoke sequence, but
        ; that may or may not be safe....
        #"(new cljs\.core\.Keyword\(null,\"[^\"]+\",\"[^\"]+\",\([0-9-]+\)\))\.cljs\$core\$IFn\$_invoke\$arity\$([0-9]+)\("
        "$1.call(null,")))


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

(defn clean-form [form]
  (postwalk ->compilable form))

(defn eval-form [engine form]
  ;; replace fn refs with our exported versions
  (let [cleaned-form (clean-form form)]

    (try
      (no-warn
        (eval-in @(.-eval-state engine)
                 cleaned-form))
      (catch :default e
        (js/console.error "Error compiling:" (str form),
                          "Cleaned: " (str cleaned-form),
                          e)
        (when-let [cause (.-cause e)]
          (js/console.error "Cause: " (.-stack cause))
          (when-let [cause2 (.-cause cause)]
            (js/console.error "Cause2: " (.-stack cause2))
            (when-let [cause3 (.-cause cause2)]
              (js/console.error "Cause3: " (.-stack cause3)))))
        (throw e)))))


; ======= Public interface ================================

(deftype JSWishEngine [eval-state]
  WishEngine
  (create-state [this] (atom {}))
  (parse-string [this s]
    (edn/read-string s #_(str "(do " s ")")))
  (eval-source-form [this state form]
    (binding [*engine-state* state]
      (eval-form this form))))

(defn create-engine []
  (->JSWishEngine (delay (create-new-eval-state))))
