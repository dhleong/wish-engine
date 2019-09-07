(ns wish-engine.runtime
  (:require [clojure.string :as str]
            [clojure.analyzer.api :refer-macros [no-warn]]
            [clojure.walk :refer [postwalk]]
            [cljs.reader :as edn]
            [cljs.js :as cljs :refer [empty-state js-eval]]
            [wish-engine.model :refer [WishEngine]]
            [wish-engine.runtime.config :as config]
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
                       (apply macro (rest sym))))

                   ; (or) and (and) don't play nicely for some reason,
                   ; so we convert them into something that works
                   (when (list? sym)
                     (let [fn-call (first sym)]
                       (if (keyword? fn-call)
                         (apply ->kw-get sym)

                         (condp = fn-call
                           'wish-engine.runtime/exported-some
                           (when (set? (second sym))
                             (->has? (rest sym)))

                           ; else, fall through:
                           nil))))

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
  (when js/goog.DEBUG
    (js/console.info "EVAL-FORM" (str form)))
  (cljs/eval state
        form
        {:eval (fn [src]
                 (js/console.warn "process..." (:source src))
                 (let [src (update src :source process-source)]
                   (try
                     (js/console.warn "EVAL" (:source src))
                     (js-eval src)
                     (catch :default e
                       (let [msg (eval-err form src e)]
                         (js/console.warn msg)
                         (throw (js/Error. msg)))))))

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
      (js/console.info "eval-form" (str form))
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
  (create-state [this] {})
  (parse-string [this s]
    (edn/read-string s #_(str "(do " s ")")))
  (eval-source-form [this state form]
    (eval-form this form)))

(defn create-engine []
  (->JSWishEngine (delay (create-new-eval-state))))
