(ns wish-engine.core
  (:require [wish-engine.model :as m]
            [wish-engine.runtime :as runtime]
            [wish-engine.scripting-api :as api]
            [wish-engine.util :as util :refer [feature-by-id throw-msg]]))

(defn- state-value [engine-state]
  (if (map? engine-state)
    engine-state
    @engine-state))

;;;
;;; entity wrapping/unwrapping for inflation

(defn- empty-entity [engine-state options]
  {:wish-engine/state (state-value engine-state)
   :wish-engine/options options})

(defn- clean-entity [entity]
  (dissoc entity :wish-engine/state :wish-engine/options))


; ======= Public interface ================================

(defn create-engine []
  (runtime/create-engine))

(defn create-state [engine]
  (m/create-state engine))

;;;
;;; initialization

(defn load-source [engine state source]
  (if (string? source)
    (let [forms (m/parse-string engine
                                (str "[" source "]"))]
      (doseq [f forms]
        (m/eval-source-form engine state f)))

    (m/eval-source-form engine state source)))

;;;
;;; entity inflation

(defn with-state [entity engine-state]
  (assoc entity :wish-engine/state (state-value engine-state)))

(defn- inflate-features [entity]
  (->> entity
       :active-features

       (map (fn [feature-info]
              (with-meta
                (merge (select-keys feature-info [:wish/instance
                                                  :wish/instance-id
                                                  :wish-engine/selected-options])
                       (feature-by-id entity (:id feature-info)))
                feature-info)))))

(defn inflate-entity
  [engine-state entity entity-state options]
  (as-> (empty-entity engine-state options) e

    (merge e entity-state)

    (if-let [apply-fn (:! entity)]
      (apply-fn e)
      e)

    (assoc e :sorted-features (inflate-features e))

    (assoc e :features (reduce
                         (fn [m feature]
                           (let [m (if-let [instance (:wish/instance-id feature)]
                                     (assoc m instance feature)
                                     m)]
                             (assoc m (:id feature) feature)))
                         {}
                         (:sorted-features e)))

    (clean-entity e)))

(defn inflate-entities
  "Given a sequence containing entities, entity IDs, and entity ID references,
   such as from `(by-id)`, `(items-from-list)`, etc. return a lazy sequence
   with all entities inflated"
  [engine-state entities]
  (let [state (state-value engine-state)]
    (mapcat
      (fn [entity]
        (util/sequentialify
          (cond
            (map? entity) entity
            (keyword? entity) ((api/by-id entity) state)
            (fn? entity) (entity state))))
      entities)))

(defn inflate-class
  [engine-state class-id entity-state options]
  (let [state (state-value engine-state)
        the-class (get-in state [:classes class-id])]
    (inflate-entity
      engine-state
      the-class
      (merge the-class entity-state)
      options)))

(defn inflate-race
  [engine-state race-id entity-state options]
  (let [state (state-value engine-state)
        the-race (or (get-in state [:races race-id])
                     (when-let [subrace (get-in state [:subraces race-id])]
                       (util/merge-entities
                         (get-in state [:races (:wish/parent-race-id subrace)])
                         subrace)))]
    (inflate-entity
      engine-state
      the-race
      (merge the-race entity-state)
      options)))

(defn inflate-list
  ([engine-state entity list-or-id] (inflate-list
                                      (with-state entity engine-state)
                                      list-or-id))
  ([state list-or-id]
   (let [list-contents (cond
                         (keyword? list-or-id)
                         (api/inflate-list
                           state
                           list-or-id)

                         (coll? list-or-id)
                         list-or-id

                         :else
                         (throw-msg
                           "You must provide either a list id or a sequence"))]
     (inflate-entities
       state
       list-contents))))
