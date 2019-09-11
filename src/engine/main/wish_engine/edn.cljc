(ns wish-engine.edn)

(defn- read-quoted [v]
  `(quote ~v))

(def edn-readers
  {(symbol "'") read-quoted
   'quote read-quoted})

