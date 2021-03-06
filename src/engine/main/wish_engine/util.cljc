(ns ^:no-doc wish-engine.util
  (:require [wish-engine.runtime.state :refer [*apply-context*]]))

(defn form? [v]
  (or (list? v)
      (seq? v)))

(def key-or-map? #(or (keyword? %)
                      (map? %)))

(defn warn [& message]
  #?(:cljs (apply js/console.warn message)
     :clj (apply println message)))

(defn throw-msg [& message]
  (throw #?(:cljs (js/Error. (apply str message))
            :clj (Exception. (apply str message)))))

(defn throw-arg
  ([fn-name arg] (throw-arg fn-name arg nil))
  ([fn-name arg reason-str]
   (let [reason (when reason-str
                  (str " (" reason-str ")"))]
     (throw-msg fn-name ": Invalid argument" reason ":\n" arg))))

(defn conj-vec [coll v]
  (cond
    (vector? coll)
    (conj coll v)

    (seq? coll)
    (conj (vec coll) v)

    (empty? coll) [v]

    :else (throw-msg "Illegal arg to conj-vec: " (type coll) ": " coll)))

(defn conj-set [coll v]
  (cond
    (set? coll)
    (conj coll v)

    (empty? coll) #{v}

    :else (throw-msg "Illegal arg to conj-vec: " (type coll) ": " coll)))

(defn feature-by-id [state id]
  (or (get-in state [:declared-features id])
      (get-in state [:features id])
      (get-in state [:wish-engine/state :features id])))

(defn entity-by-id [state id]
  (or (feature-by-id state id)
      (get-in state [:list-entities id])
      (get-in state [:wish-engine/state :list-entities id])
      (warn "Could not find entity with ID " id)))

(defn instance-id [feature-id container-id instance-n]
  (keyword (namespace feature-id)
           (str (name feature-id)
                "#"
                (name container-id)
                "#"
                instance-n)))

(defn merge-item-spec [base item]
  (merge-with
    (fn [a b]
      (cond
        (map? a)
        (merge a b)

        (coll? a)
        (concat a b)

        :else b))
    base item))

(defn merge-entities [a b]
  (when-not (and (map? a) (map? b))
    (throw (ex-info "Arguments to merge-entities must both be maps"
                    {:a a :b b})))

  (merge-with
    (fn [a b]
      (cond
        (and (map? a) (map? b))
        (merge-entities a b)

        ; non-heterogeneous... just pick newer
        (or (map? a) (map? b))
        b

        (coll? a)
        (concat a b)

        (and (fn? a)
             (fn? b))
        (comp b a)

        :else b))
    a b))

(defn sequentialify
  "If `v` is already sequential, return it directly;
   otherwise, wrap it in a vector"
  [v]
  (if (sequential? v)
    v
    [v]))

(defn- option-by-id [state feature option-id]
  (or (some->> feature
               :values
               (mapcat (fn [v]
                         (cond
                           (map? v) [v]
                           (ifn? v) (v state))))
               (filter #(= option-id (:id %)))
               first)

      (get-in state [:options (:id feature) option-id])
      (get-in state [:wish-engine/state :options (:id feature) option-id])

      ; could be some other entity kind
      (entity-by-id state option-id)

      (throw-msg "Could not find option: " option-id
                 " for feature: " (:id feature)
                 " (providing: " *apply-context* ")")))

(defn selected-options [state feature]
  (let [feature-id (or (:wish/instance-id feature)
                       (:id feature))
        option-value (get-in state [:wish-engine/options feature-id])
        option-ids (if (and (map? option-value)
                            (:value option-value))
                     ; instanced feature, probably
                     (:value option-value)

                     ; normal case
                     option-value)]
    (some->> option-ids
             seq
             (map (partial option-by-id state feature)))))
