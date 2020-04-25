(ns wish-engine.runtime.js
  "Macros for exposing fns to template functions"
  (:require [clojure.string :as str]
            [wish-engine.runtime.config :as config]))

; NOT exhaustive, but should cover our uses:
(def ^:private reserved?
  #{"if" "let" "for"})

(defn- ->js-name [n]
  (if (reserved? n)
    (str n "$")
    (str/replace
      n
      #"[+-/*?<>=]"
      (fn [ch]
        (case ch
          "." "."
          "+" "_PLUS_"
          "-" "_"
          "/" "_SLASH_"
          "*" "_STAR_"
          "?" "_QMARK_"
          ">" "_GT_"
          "<" "_LT_"
          "=" "_EQ_")))))

(defn cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:ns env)))

(defmacro when-cljs
  "Return `body` in an implicit `do` if we are generating cljs code
   https://groups.google.com/d/msg/clojurescript/iBY5HaQda4A/w1lAQi9_AwsJ"
  [& body]
  (when (cljs-env? &env)
    (cons 'do body)))

(defn set-assoc-stmt [map-name assoc-key assoc-value]
  `(when-cljs
     (set! ~map-name
           (assoc ~map-name
                  ~assoc-key
                  ~assoc-value))))

(defn export-fn-symbol-stmt [n exported-symbol]
  (let [exported-map-name 'exported-fns
        this-ns-name (-> *ns* ns-name name)
        js-name (str (->js-name this-ns-name) "."
                     (->js-name (name exported-symbol)))]
    `(when-cljs
       ~(set-assoc-stmt exported-map-name
                        (if (string? n)
                          `(symbol ~n)
                          `(symbol ~(name n)))
                        `(symbol
                           ~this-ns-name
                           ~(name exported-symbol)))
       (when-not js/goog.DEBUG
         (~'js/goog.exportSymbol ~js-name ~exported-symbol)))))

(defmacro export-fn
  [fn-symbol & [?apply-to-args]]
  (let [n (name fn-symbol)
        exported-symbol (symbol (str "exported-"
                                     (str/replace n #"/" "_SLASH_")))
        core-ns (or #?(:clj (some-> fn-symbol resolve meta :ns ns-name name))
                    "cljs.core")
        core-ns-symbol (symbol core-ns n)]
    `(do
       (defn ^:export ~exported-symbol
         [& ~'args]
         ~(if ?apply-to-args
            `(apply ~core-ns-symbol (~?apply-to-args ~'args))
            `(apply ~core-ns-symbol ~'args))) 
       ~(export-fn-symbol-stmt n exported-symbol))))

(defmacro export-macro
  "Ensure a cljs.core macro is exported"
  [macro-sym & [conditional?]]
  (let [export `(~'js/goog.exportSymbol
                  ~(str (name config/runtime-eval-ns) "$macros."
                        (->js-name (name macro-sym)))
                  ~(symbol (str "cljs.core$macros/"
                                (name macro-sym))))]
    (if conditional?
      `(when-not js/goog.DEBUG
         ~export)
      export)))

(defmacro export-sym [sym]
  (let [n (name sym)
        core-ns (if-let [sym-meta #?(:clj (-> sym resolve meta)
                                     :cljs nil)]
                  (-> sym-meta :ns ns-name name)
                  "cljs.core")]
    `(~'js/goog.exportSymbol
       ~(str core-ns "." (->js-name n))
       ~(symbol core-ns n))))

