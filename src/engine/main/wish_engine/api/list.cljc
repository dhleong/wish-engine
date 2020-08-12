(ns ^:no-doc wish-engine.api.list
  (:require [wish-engine.util :as util :refer [throw-msg]]))

(def ^{:dynamic true
       :private true}
  *list-inflation-context* nil)

(defn inflate [state list-id]
  (when (= list-id *list-inflation-context*)
    (throw-msg "Infinite recursion detected: inflating " list-id))

  (binding [*list-inflation-context* list-id]
    (some->> (concat (get-in state [:lists list-id])
                     (get-in state [:wish-engine/state :lists list-id]))
             seq
             (mapcat (fn [entry]
                       (when-let [results (cond
                                            ;; *should* be handled by
                                            ;; add-to-list*, but just in
                                            ;; case...
                                            (keyword? entry) (util/entity-by-id state entry)
                                            (fn? entry) (entry state)
                                            :else entry)]
                         (util/sequentialify results)))))))
