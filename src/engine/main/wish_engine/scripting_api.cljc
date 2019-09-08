(ns wish-engine.scripting-api
  (:require [wish-engine.runtime.api :refer-macros [defn-api]]))

(def exported-fns {})

; ======= Public API ======================================

(defn-api has?
  "Alias for (some) that can handle sets in production"
  [vals-set coll]
  (some
    (fn [item]
      (contains? vals-set item))
    coll))

(defn-api ordinal [n]
  (str n
       (if (<= 11 n 19)
         "th"
         (let [ones (rem n 10)]
           (case ones
             1 "st"
             2 "nd"
             3 "rd"
             "th")))))
