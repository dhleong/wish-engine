(ns wish-engine.converter.print
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]))

(defn pprint-form [form]
  (let [s (with-out-str
            (binding [pprint/*print-right-margin* 100
                      pprint/*print-miser-width* 100
                      pprint/*print-pretty* true
                      *print-namespace-maps* false]
              (pprint/with-pprint-dispatch pprint/code-dispatch
                (pprint/pprint form))))]
    (-> s
        (str/replace "\\n" "\n")
        print)))


