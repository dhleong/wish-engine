(ns wish-engine.util)

(def key-or-map? #(or (keyword? %)
                      (map? %)))

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
      (get-in state [:wish-engine/state :features id])))

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
  (merge-with
    (fn [a b]
      (cond
        (map? a)
        (merge-entities a b)

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
                           (ifn? v) (v (:wish-engine/state state)))))
               (filter #(= option-id (:id %)))
               first)

      (get-in state [:options (:id feature) option-id])
      (get-in state [:wish-engine/state :options (:id feature) option-id])

      (throw-msg "Could not find option: " option-id
                 " for feature: " (:id feature))))

(defn selected-options [state feature]
  (let [feature-id (or (:wish/instance-id feature)
                       (:id feature))
        option-ids (get-in state [:wish-engine/options feature-id])]
    (some->> option-ids
             seq
             (map (partial option-by-id state feature)))))
