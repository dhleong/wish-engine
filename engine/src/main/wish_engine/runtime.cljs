(ns wish-engine.runtime
  (:require [clojure.string :as str]
            [clojure.analyzer.api :refer-macros [no-warn]]
            [cljs.reader :as edn]
            [cljs.js :refer [empty-state eval js-eval]]))

(def ^:private runtime-eval-ns 'wish-engine.runtime-eval)

(defn- process-source [js-src]
  (-> js-src
      (str/replace
        ; we could also just replace the _invoke sequence, but
        ; that may or may not be safe....
        #"(new cljs\.core\.Keyword\(null,\"[^\"]+\",\"[^\"]+\",\([0-9-]+\)\))\.cljs\$core\$IFn\$_invoke\$arity\$([0-9]+)\("
        "$1.call(null,")))

(defn- eval-in [state form]
  (eval state
        form
        {:eval (fn [src]
                 (let [src (update src :source process-source)]
                   (try
                     (js-eval src)
                     (catch :default e
                       (js/console.warn (str "FAILED to js/eval:\n\n"
                                             (:source src)
                                             "\n\nOriginal error: " (.-stack e)))
                       (throw (js/Error.
                                (str "FAILED to js/eval:\n\n"
                                     (:source src)
                                     "\n\nOriginal error: " (.-stack e))))))))

         :context :expr
         :ns runtime-eval-ns}
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
        'ns runtime-eval-ns
        '(:require [wish-engine.runtime])))

    new-state))

(defn clean-form
  [form]
  form ; TODO
  #_(postwalk ->compilable form))

(defn eval-form [engine form]
  ;; replace fn refs with our exported versions
  (let [cleaned-form (clean-form form)]

    (try
      (no-warn
        (eval-in @(:eval-state engine)
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

(defn create-engine []
  {:eval-state (delay (create-new-eval-state))})

(defn read-string [s]
  (edn/read-string (str "(do " s ")")))
