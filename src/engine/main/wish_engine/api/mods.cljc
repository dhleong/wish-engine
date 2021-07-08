(ns wish-engine.api.mods)

(defn apply-mod-to-entity [entity mod-fn]
  (when-not (meta mod-fn)
    (throw (ex-info "Invalid mod-fn (no meta)"
                    {:fn mod-fn
                     :apply-to entity})))

  (let [{:keys [mod-id]} (meta mod-fn)
        existing-meta (meta entity)
        applied? (mod-id (:mods existing-meta))]
    (if applied?
      entity
      (with-meta
        (mod-fn entity)
        (update existing-meta :mods (fnil conj #{}) mod-id)))))

(defn apply-mods-to-entity [entity mods-coll]
  (reduce
    apply-mod-to-entity
    entity
    mods-coll))

(defn install-mod-on-type [entities-map entity-id mod-fn]
  (if (entity-id entities-map)
    (update entities-map entity-id apply-mod-to-entity mod-fn)
    entities-map))

(defn install [state entity-id mod-fn]
  (->> (keys state)
       (remove #{:mods})

       (reduce
         (fn [s entity-kind]
           (update s entity-kind install-mod-on-type entity-id mod-fn))
         state)))

(defn with-mods [state entity]
  (let [mods (vals (get-in state [:mods (:id entity)]))]
    (apply-mods-to-entity entity mods)))
