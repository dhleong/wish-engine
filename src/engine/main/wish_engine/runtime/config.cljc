(ns wish-engine.runtime.config)

(def runtime-eval-ns 'wish-engine.runtime-eval)
(def runtime-export-ns 'wish-engine.runtime)

(defn with-exported-ns
  "Simply creates a Symbol named by the given symbol,
   but in the runtime-eval namespace (wish-engine.runtime-eval)"
  [name-sym]
  (symbol (name runtime-eval-ns) (str name-sym)))

(defn exported-fqn
  "Given the name of an exported fn, return a fully-qualified
   symbol that includes he `exported-` prefix."
  [name-sym]
  (with-exported-ns
    (str "exported-" (name name-sym))))
