(ns wish-engine.api.features
  (:require [wish-engine.runtime.state :refer [*apply-context*]]
            [wish-engine.util :as util
             :refer [conj-vec throw-arg throw-msg]]))

; ======= utils ===========================================

(defn- extract-feature-id [feature-or-id]
  (cond
    (map? feature-or-id) (:id feature-or-id)
    (keyword? feature-or-id) feature-or-id
    :else (throw-arg "provide-feature" feature-or-id
                     "feature ID or map")))


; ======= Validation and compilation ======================

(defn validate-map [m]
  (letfn [(throw-reason [& args]
            (throw-msg "Invalid feature ("
                       (apply str args)
                       "):\n"
                       m))]
    (when-not (:id m)
      (throw-reason "missing :id"))

    ; return if valid
    m))

(defn compile-map [m]
  (as-> m m
    ; add level-scaling to the :! fn
    (if-not (:levels m) m
      (assoc m :! (let [{:keys [levels] existing-fn :!} m]
                    (fn apply-fn [state]
                      (let [state (if existing-fn
                                    (existing-fn state)
                                    state)
                            current-level (:level state)]
                        (reduce-kv
                          (fn [state level {level-fn :!}]
                            (if (and (ifn? level-fn)
                                     (>= current-level level))
                              (level-fn state)
                              state))
                          state
                          levels))))))

    (if-not (:values m) m
      (update m :values
              (partial map (fn [option]
                             (cond
                               (map? option)
                               (compile-map option)

                               (keyword? option)
                               option

                               :else
                               (throw-msg "Invalid :value option to "
                                          (:id m)
                                          ": " option))))))

    ; wrap to provide *apply-context* so we can potentially track what
    ; feature/option/etc provided what
    (if-not (:! m) m
      (update m :! (fn [existing-fn]
                     (when existing-fn
                       (vary-meta

                         (fn apply-fn [state]
                           (binding [*apply-context* (:id m)]
                             (existing-fn state)))

                         assoc :wish-engine/source (:id m))))))))


; ======= Provide handling ================================

(defn- install-instance-info [feature apply-context state]
  (let [id (:id feature)
        instanced? (:instanced? feature)
        next-instance (get-in state [:wish/instances id] 0)]

    (when (and (not instanced?)
               (> next-instance 0))
      (js/console.warn "Adding duplicate feature " id
                       " from " apply-context
                       " but it is not instanced"))

    (if-not instanced?
      feature
      (assoc feature
             :wish/instance next-instance
             :wish/instance-id (util/instance-id
                                 id
                                 (:id state)
                                 next-instance)))))

(defn- declare-feature [state feature declared-inline?]
  (if-not declared-inline?
    state
    (assoc-in state [:declared-features (:id feature)] feature)))

(defn- create-active-features-entry
  [apply-context id with-instance selected-options]
  (as-> {:id id} instance

    ; attach the context
    (if-let [ctx apply-context]
      (assoc instance :wish-engine/source ctx)
      instance)

    ; attach selected-options
    (if-not selected-options instance
      (assoc instance
             :wish-engine/selected-options selected-options))

    ; attach instance info
    (if-not (:instanced? with-instance) instance
      (merge (select-keys with-instance [:wish/instance
                                         :wish/instance-id])
             instance))))

(defn- apply-options [state selected-options]
  (if-not selected-options state
    (reduce
      (fn [s option]
        (if-let [apply-fn (:! option)]
          (apply-fn s)
          s))
      state
      selected-options)))

(defn- on-provide [state feature selected-options]
  (as-> state state
    (if-let [apply-fn (:! feature)]
      (apply-fn state)
      state)

    (apply-options state selected-options)))

(defn provide [apply-context state feature]
  (let [id (extract-feature-id feature)
        declared-inline? (map? feature)

        feature (if declared-inline?
                  (->> feature validate-map compile-map)
                  (util/feature-by-id state id))

        ; prepare for instancing
        with-instance (install-instance-info
                        feature apply-context state)

        ; inflate selected options
        selected-options (util/selected-options state with-instance)]

    (-> state

      ; declare the feature on the entity state, if a map
      (declare-feature feature declared-inline?)

      ; record the new instance
      (update-in [:wish/instances id] inc)

      ; install in :active-features
      (update :active-features conj-vec
              (create-active-features-entry
                apply-context id with-instance selected-options))

      ; trigger any on-provide (:!) fn on the feature and any of its
      ; selected optiosn
      (on-provide feature selected-options))))
