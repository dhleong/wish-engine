(ns wish-engine.core
  (:require [wish-engine.runtime :as runtime]
            [wish-engine.util :refer [feature-by-id]]))

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

;;;
;;; entity inflation

(defn- inflate-features [entity]
  (->> entity
       :active-features

       ; TODO expand :instanced features? warn of duplicates?
       (map (fn [feature-info]
              (with-meta
                (merge (select-keys feature-info [:wish/instance
                                                  :wish/instance-id])
                       (feature-by-id entity (:id feature-info)))
                feature-info)))))

(defn inflate-entity
  [engine-state entity entity-state options]
  (as-> (empty-entity engine-state options) e

    (merge e entity-state)

    (if-let [apply-fn (:! entity)]
      (apply-fn e)
      e)

    (assoc e :features (inflate-features e))

    (clean-entity e)))
