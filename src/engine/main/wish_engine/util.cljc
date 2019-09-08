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
