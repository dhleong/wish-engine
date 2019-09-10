(ns wish-engine.util)

(defn throw-msg [& message]
  (throw #?(:cljs (js/Error. (apply str message))
            :clj (Exception. (apply str message)))))

(defn conj-vec [coll v]
  (cond
    (vector? coll)
    (conj coll v)

    (seq? coll)
    (conj (vec coll) v)

    (empty? coll) [v]

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

(defn- option-by-id [state feature option-id]
  (or (some->> feature
               :values
               (filter #(= option-id (:id %)))
               first)

      (get-in state [:options (:id feature) option-id])
      (get-in state [:wish-engine/state :options (:id feature) option-id])))

(defn selected-options [state feature]
  (let [option-ids (get-in state [:wish-engine/options (:id feature)])]
    (some->> option-ids
             seq
             (map (partial option-by-id state feature)))))
