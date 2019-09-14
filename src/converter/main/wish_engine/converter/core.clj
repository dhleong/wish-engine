(ns wish-engine.converter.core
  (:require [clojure.string :as str]
            [clojure.walk :refer [prewalk]]))

(declare convert)

(defn- convert-map [m]
  (reduce-kv
    (fn [m k v]
      (case k
        (:features :+features)
        (assoc m :! `(~'on-state (~'provide-features ~@v)))

        (:! :+!)
        (if (= 'on-state (first v))
          m
          (assoc m :! `(~'on-state ~@(convert v))))

        :&levels
        (assoc m :levels v)

        :values
        (assoc m :values (cond
                           (keyword? v) `(~'items-from-list ~v)
                           (and (vector? v)
                                (= 1 (count v))
                                (keyword? (first v)))
                           `(~'items-from-list ~(first v))

                           :else v))

        :5e/starting-eq
        (assoc m :5e/starting-eq `(~'quote ~v))

        ; otherwise, keep as-is
        (assoc m k v)))
    (if (every? number? (keys m))
      (sorted-map)
      {})
    m))

(defn- convert-vector [v]
  (cond
    (and (keyword? (first v))
         (str/starts-with? (name (first v)) "!declare-"))
    `(~(symbol (subs (name (first v)) 1))
               ~@(rest v))

    (= :!provide-options (first v))
    `(~'declare-options ~@(rest v))

    (#{:!provide-feature
       :!provide-features
       :!provide-attr
       :!add-to-list
       :!add-limited-use} (first v))
    `(~(symbol (subs (name (first v)) 1)) ~@(rest v))

    (= :!provide-features (first v))
    `(~'provide-features ~@(rest v))

    :else v))

(defn- convert-subform [form]
  (cond
    (map? form) (convert-map form)
    (vector? form) (convert-vector form)
    :else form))

(defn convert [source-forms]
  (map (partial prewalk convert-subform)
       source-forms))
