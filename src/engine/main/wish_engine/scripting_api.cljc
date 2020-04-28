(ns wish-engine.scripting-api
  "Public scripting API

   The public functions in this namespace (declared with `defn-api`) are
   provided at runtime to wish-engine Data Source scripts. Data Source scripts
   have two modes of operation: compile-time and runtime.

   Compile-time, or \"top-level,\" operation occurs when the Data Source is
   initially loaded, and is used to declare all the primary entities that might
   be used to build a character. As such, all compile-time functions are named
   with the `declare-` prefix, such as `declare-class`.  Compile-time functions
   may *only* be used at compile-time.

   Runtime operation occurs when a character sheet is in use, with the purpose
   of building upon a primary entity (such as a Class) based on user-selected
   options, in order to build up the \"current\" state of the entity. Runtime
   functions are invoked on an entity state map, and return the new state,
   allowing them to be used easily in pipelines to gradually add features, etc.

   In general, any feature can have a runtime operation attached to it (also
   known as an `on-provide` function, using the `:!` key) which will be applied
   to an entity state when the feature is attached it. Such functions have a
   simple signature:

     (fn [state] state)

   To simplify implementation, the `on-state` macro is provided, which
   automatically threads the given state through. It might be used like:

     (on-state
       (provide-feature :gunslinging)
       (provide-to-list :weapons :weapon/captains-pistol))

   In general, when providing things to an entity, whether features to an
   entity, or items to a list, you can use:

     1. A function of `state` that returns the value: This enables you to
        refer to values that may be declared in other Data Sources, or which
        are dynamically added by other features at runtime. Several helpers for
        creating these functions exist, such as [options-of] or [items-from-list].
     2. A map: this represents an inline declaration of the feature at runtime,
        as you provide it. This is commonly used for class features that won't
        be referenced by other features.
     3. A keyword: this is syntactic sugar for using the [by-id] function,
        enabling you to easily reference a feature declared elsewhere.

   In addition, for any `provide` or `declare` function that allows you to pass
   multiple values, you can provide a mix of sequences and values, enabling you
   to use strutures like `(map)` or list comprehension with `(for)` to generate
   values."
  (:require #?(:clj [wish-engine.runtime.api :refer [defn-api]]
               :cljs [wish-engine.runtime.api :refer-macros [defn-api]])
            [wish-engine.api.util :refer [flatten-lists]]
            [wish-engine.runtime.state :refer [*engine-state* *apply-context*]]
            [wish-engine.util :as util :refer [conj-set key-or-map?
                                               throw-arg throw-msg
                                               warn]]
            [wish-engine.api.attr :as attr]
            [wish-engine.api.features :as features]
            [wish-engine.api.limited-use :as limited-use]
            [wish-engine.api.list :as list]))


(def ^:no-doc exported-fns {})
(def ^:no-doc exported-fn-refs {})


; ======= utils ===========================================

(defn- ->map [entities]
  (reduce (fn [m f]
            (assoc m (:id f) f))
          {}
          entities))


; ======= Util API ========================================

(defn-api has?
  "Alias for (some) that can handle sets in production"
  [vals-set coll]
  (some
    (fn [item]
      (contains? vals-set item))
    coll))

(defn-api ordinal
  "Given an integer, returns the English ordinal string, EG: 1 -> 1st, 2 -
   >2nd, etc."
  [n]
  (str n
       (if (<= 11 n 19)
         "th"
         (let [ones (rem n 10)]
           (case ones
             1 "st"
             2 "nd"
             3 "rd"
             "th")))))


; ======= Function factories ==============================

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
    (or (list/inflate state list-id)
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

(defn-api provide-limited-use
  "Provide a limited-use item, stored at `[:limited-uses id]` in the
   entity. wish-engine does little with these directly, but as features
   that may only be used a limited number of times are core to most
   RPG systems, limited-use is a core function of wish.

   Here's a real example from Wish:

     (provide-limited-use
       {:id :sorcerer/points#uses
        :name \"Sorcery Points\"
        :uses (fn [#{level}] level)
        :restore-trigger (fn [#{level}]
                           (if (< 20 level)
                             :long-rest
                             :short-rest))
        :restore-amount
          (fn [#{trigger used level}]
            (if (= :short-rest trigger)
              (when (= 20 level) (min used 4))
              used))})

   Note the naming with the providing feature followed by the `#uses`
   suffix. This is not required, but is a common convention. The entity
   state will be passed to both the `:uses` and `:restore-amount` functions,
   enabling you to scale uses programmatically. They may both alternatively
   be declared statically. wish-engine will compile `:restore-amount` to a
   function, and even provide a default value that restores everything, if
   omitted."
  [state spec]
  (when *engine-state*
    (throw-msg "add-limited-use must not be called at the top level."))

  (update state :limited-uses
          assoc (:id spec)
          (->> spec
               limited-use/validate-spec
               limited-use/compile-spec)))

(defn-api add-limited-use
  "Legacy alias for `provide-limited-use`"
  [state spec]
  (provide-limited-use state spec))


;;;
;;; Provide attr
;;;

(defn-api provide-attr
  "Provide an attribute to the entity, explictly. Attributes are used for
   sheet-specific data, and can store permanent or temporary state. The path
   can be a simple keyword or a sequence of keywords, and the value may be of
   any type.

   A common pattern for providing multiple values for an attribute is to share
   a top-level key and store values in a sub-key based on the feature. For
   example, an item with ID `:armor/browncoat` that provides a +2 bonus to AC
   when equipped might look like:

     {:id :armor/browncoat
      :name \"An old coat from another time\"
      :! (on-state
           (provide-attr [:buffs :ac :armor/browncoat] 2))}

   Then in order to compute the total AC bonuses at runtime, client code can
   do something like:

     (->> (get-in entity [:attrs :buffs :ac])
          vals
          (apply +))

   This structure allows on-provide functions to provide attributes in
   an idempotent way.

   The ID of the feature that provided this attribute (IE :armor/browncoat
   in the above example) will be stored at `[:attrs/meta ...path]`."
  [state attr-id-or-path value]
  (when *engine-state*
    (throw-msg "provide-attr must not be called at the top level."))

  (attr/provide state attr-id-or-path value))

;;;
;;; Provide Feature
;;;

(defn-api provide-feature
  "Provide a Feature to the entity. `feature` may be an ID keyword or a map,
   declaring the feature inline. See `declare-feature`"
  [state feature]
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

(defn-api provide-to-list
  "Provide one or more entries to a list. The list may either be declared by id or
   with a list spec, which must be of the format:

     {:id :my-list
      :type :up-to-you}

   If the `:type` is `:feature`, each map item of `entries` will be treated
   as `(provide-feature)`.
   "
  [state id-or-spec & entries]
  (when *engine-state*
    (throw-msg "add-to-list must not be called at the top level. Try `declare-list`"))

  (add-to-list* state "add-to-list" id-or-spec entries))

(defn-api add-to-list
  "Legacy alias of `provide-to-list`"
  [state id-or-spec & entries]
  (apply provide-to-list state id-or-spec entries))

(defn-api declare-list [id-or-spec & entries]
  (when-not *engine-state*
    (throw-msg "declare-list must be called at the top level. Try `add-to-list`"))
  (swap! *engine-state* add-to-list* "declare-list" id-or-spec entries))
