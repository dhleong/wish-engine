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

(defmacro export-fn
  [fn-symbol & [?apply-to-args]]
  (let [n (name fn-symbol)
        exported-map-name 'exported-fns
        this-ns-name (-> config/runtime-eval-ns
                         name
                         ->js-name)
        exported-name (str "exported-" n)
        exported-symbol (symbol (str "exported-"
                                     (str/replace n #"/" "_SLASH_")))
        js-name (str this-ns-name "."
                     (->js-name exported-name))
        core-ns (or #?(:clj (some-> fn-symbol resolve meta :ns ns-name name))
                    "cljs.core")
        core-ns-symbol (symbol core-ns n)]
    `(do
       (defn ^:export ~exported-symbol
         [& ~'args]
         ~(if ?apply-to-args
            `(apply ~core-ns-symbol (~?apply-to-args ~'args))
            `(apply ~core-ns-symbol ~'args)))
       (when-not js/goog.DEBUG
         (~'js/goog.exportSymbol ~js-name ~exported-symbol))
       (set! ~exported-map-name
             (assoc ~exported-map-name (symbol ~n) (symbol
                                                     ~this-ns-name
                                                     ~(name exported-symbol)))))))

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

