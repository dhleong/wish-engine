(ns wish-engine.converter
  (:import (java.io File))
  (:require [clojure.java.io :as io]
            [wish-engine.converter.core :refer [convert]]
            [wish-engine.converter.print :refer [pprint-form]]))

(defn read-forms [^File f]
  (read-string
    (str "[" (slurp f) "]")))

(defn convert-file [^File source, ^File dest]
  (when-not (.exists source)
    (throw (IllegalArgumentException.
             (str "Source " source " does not exist"))))

  (-> source
      read-forms
      convert
      (as-> converted
        (with-open [out (io/writer dest)]
          (binding [*out* out]
            (doseq [c (interpose nil converted)]
              (if (nil? c)
                (println) ; whitespace please
                (pprint-form c))))))))

(defn -main [from to & _]
  (convert-file (File. from)
                (File. to)))
