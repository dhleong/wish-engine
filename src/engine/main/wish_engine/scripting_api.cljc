(ns wish-engine.scripting-api
  "Public scripting API"
  (:require [wish-engine.runtime.api :refer-macros [defn-api]]
            [wish-engine.runtime.state :refer [*engine-state* *apply-context*]]
            [wish-engine.util :as util :refer [conj-set key-or-map?
                                               throw-arg throw-msg
                                               warn]]
            [wish-engine.api.attr :as attr]
            [wish-engine.api.features :as features]))


(def exported-fns {})
(def exported-fn-refs {})

(def ^{:dynamic true
       :private true}
  *list-inflation-context* nil)


; ======= utils ===========================================

(defn flatten-lists
  "Call on a varargs list to support both the usual varargs invocation and
   passing in sequences, such as might be generated by `(map)` or a list
   comprehension"
  [fn-name valid? args]
  (mapcat (fn [f]
            (cond
              (and (sequential? f)
                   (every? valid? f)) f
              (and (not (sequential? f))
                   (valid? f)) [f]
              :else (throw-arg fn-name f)))
          args))

(defn- ->map [entities]
  (reduce (fn [m f]
            (assoc m (:id f) f))
          {}
          entities))

; ======= Validation ======================================

(defn validate-limited-use-spec [s]
  (letfn [(throw-reason [& args]
            (throw-msg "Invalid limited-use ("
                       (apply str args)
                       "):\n"
                       s))]
    (when-not (:id s)
      (throw-reason "missing :id"))

    (when-let [amount (:restore-amount s)]
      (when-not (or (number? amount)
                    (ifn? amount))
        (throw-reason ":restore-amount must be a number or a fn")))

    ; return if valid
    s))


; ======= Compilation =====================================

(defn compile-limited-use-spec [s]
  (as-> s s

    ; ensure :restore-amount is always a fn
    (if (ifn? (:restore-amount s)) s
      ;; if not provided, restore all
      (update s :restore-amount (fn [original]
                                  (cond
                                    ;; constant restore amount
                                    (number? original)
                                    (constantly original)

                                    ;; restore all if not otherwise specified
                                    (nil? original)
                                    (fn restore-all [{:keys [used]}]
                                      used)))))))


; ======= Util API ========================================

(defn-api has?
  "Alias for (some) that can handle sets in production"
  [vals-set coll]
  (some
    (fn [item]
      (contains? vals-set item))
    coll))

(defn-api ordinal [n]
  (str n
       (if (<= 11 n 19)
         "th"
         (let [ones (rem n 10)]
           (case ones
             1 "st"
             2 "nd"
             3 "rd"
             "th")))))


; ======= top-level forms =================================

(defn- declare-toplevel [fn-name path features]
  (when-not *engine-state*
    (throw-msg fn-name " must be called at the top level"))

  (swap! *engine-state*
         update-in
         path
         merge

         (->> features

              (flatten-lists fn-name map?)
              (map features/validate-map)
              (map features/compile-map)

              ->map)))

(defn-api declare-class [class-spec]
  (declare-toplevel "declare-class" [:classes] [class-spec]))

(defn-api declare-effects [& effects]
  (declare-toplevel "declare-effects" [:effects] effects))

(defn-api declare-features [& features]
  (declare-toplevel "declare-features" [:features] features))

(defn-api declare-items [base & items]
  (declare-toplevel "declare-items" [:items]
                    (map (partial util/merge-item-spec base)
                         items)))

(defn-api declare-options [feature-id & options]
  (when-not (keyword? feature-id)
    (throw-arg "declare-options" feature-id
               "feature id keyword"))
  (declare-toplevel "declare-options" [:options feature-id] options))

(defn-api declare-race [race-spec]
  (declare-toplevel "declare-race" [:races] [race-spec]))

(defn-api declare-subrace [parent-race-id race-spec]
  (when-not (keyword? parent-race-id)
    (throw-arg "declare-subrace" parent-race-id
               "parent race id keyword"))

  (let [race-id (:id race-spec)]
    (declare-toplevel "declare-race"
                      [:subraces]
                      [(assoc race-spec
                              :wish/parent-race-id parent-race-id)])
    (swap! *engine-state*
           update-in
           [:subraces-of parent-race-id]
           conj-set race-id)))


; ======= Entity-modifying forms ==========================

;;;
;;; Limited use
;;;

(defn-api add-limited-use [state spec]
  (when *engine-state*
    (throw-msg "add-limited-use must not be called at the top level."))

  (update state :limited-uses
          assoc (:id spec)
          (->> spec
               validate-limited-use-spec
               compile-limited-use-spec)))


;;;
;;; Provide attr
;;;

(defn-api provide-attr [state attr-id-or-path value]
  (when *engine-state*
    (throw-msg "provide-attr must not be called at the top level."))

  (attr/provide state attr-id-or-path value))

;;;
;;; Provide Feature
;;;

(defn-api provide-feature [state feature]
  (when *engine-state*
    (throw-msg "provide-feature(s) must not be called at the top level. Try `declare-features`"))

  (features/provide *apply-context* state feature))

(defn-api provide-features [state & features]
  (->> features

       (flatten-lists
         "provide-features"
         key-or-map?)

       (reduce provide-feature
               state)))


; ======= list handling ===================================

(defn inflate-list [state list-id]
  (when (= list-id *list-inflation-context*)
    (throw-msg "Infinite recursion detected: inflating " list-id))

  (binding [*list-inflation-context* list-id]
    (some->> (concat (get-in state [:lists list-id])
                     (get-in state [:wish-engine/state :lists list-id]))
             seq
             (mapcat (fn [entry]
                       (when-let [results (if (fn? entry)
                                            (entry state)
                                            entry)]
                         (util/sequentialify results)))))))

(defn-api by-id
  "Given an ID, returns a function of `state` that will fetch
   the feature (or list item) with that ID"
  [id]
  (when-not (keyword? id)
    (throw-arg "by-id" id
               "entity id keyword"))

  (fn entity-by-id [state]
    (util/entity-by-id state id)))

(defn-api items-from-list
  "Given a list ID, returns a function of `state` that will fetch
   the items from that list"
  [list-id]
  (when-not (keyword? list-id)
    (throw-arg "items-from-list" list-id
               "list id keyword"))

  (fn list-items-by-id [state]
    (or (inflate-list state list-id)
        (warn "Could not find list with ID " list-id))))

(defn-api options-of
  "Given a feature ID, returns a function of `state` that fetches the
   selected options of that feature"
  [feature-id]
  (when-not (keyword? feature-id)
    (throw-arg "options-of" feature-id
               "list id keyword"))

  (fn feature-options-by-id [state]
    (or (when-let [f (util/feature-by-id state feature-id)]
          ; normal case
          (util/selected-options state f))

        ; implicit features, like spellcaster spell lists
        (util/selected-options state {:id feature-id})

        ; nothing
        (warn "Could not find feature: " feature-id))))

(defn- add-to-list*
  [state fn-name id-or-spec entries]
  (let [id (if (keyword? id-or-spec)
             id-or-spec
             (:id id-or-spec))
        spec (when (map? id-or-spec)
               (dissoc id-or-spec :id))
        dest-key (if (= :feature (:type spec))
                   :features
                   :list-entities)
        entries (flatten-lists fn-name identity entries)
        dest-entries (if (= :feature (:type spec))
                       (->> entries
                            (filter map?)
                            (map (comp features/compile-map
                                       features/validate-map)))
                       entries)
        inflatable-entries (map (fn [e]
                                  (cond
                                    (map? e) (by-id (:id e))
                                    (ifn? e) e  ; easy case
                                    (keyword? e) (by-id e)
                                    :else (throw-arg fn-name e
                                                     (str "id, map, or functional"
                                                          "; was " (or (type e)
                                                                       "nil")))))
                                entries)]

    (-> state
        (update dest-key merge (->map dest-entries))
        (update-in [:lists id] concat inflatable-entries))))

(defn-api add-to-list
  [state id-or-spec & entries]
  (when *engine-state*
    (throw-msg "add-to-list must not be called at the top level. Try `declare-list`"))

  (add-to-list* state "add-to-list" id-or-spec entries))

(defn-api declare-list [id-or-spec & entries]
  (when-not *engine-state*
    (throw-msg "declare-list must be called at the top level. Try `add-to-list`"))
  (swap! *engine-state* add-to-list* "declare-list" id-or-spec entries))
