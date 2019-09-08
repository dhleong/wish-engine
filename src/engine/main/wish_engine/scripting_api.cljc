(ns wish-engine.scripting-api
  "Public scripting API"
  (:require [wish-engine.runtime.api :refer-macros [defn-api]]))


(def exported-fns {})

(def ^:dynamic *engine-state* (atom {}))

; ======= utils ===========================================

(defn throw-msg [& message]
  (throw #?(:cljs (js/Error. (apply str message))
            :clj (Exception. (apply str message)))))

(defn throw-arg [fn-name arg]
  (throw-msg "Invalid argument to " fn-name ": " arg))

; ======= Util API ========================================

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


; ======= declare- forms ==================================

(defn-api declare-features [& features]
  (swap! *engine-state*
         update
         :features
         merge

         (->> features

              ; support passing in sequences, such as generated
              ; by map or a list comprehension
              (mapcat (fn [f]
                        (cond
                          (map? f) [f]
                          (seq? f) f
                          :else (throw-arg "declare-features" f))))

              (reduce (fn [m f]
                        (assoc m (:id f) f))))))
