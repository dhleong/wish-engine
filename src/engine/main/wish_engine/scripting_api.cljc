(ns wish-engine.scripting-api
  "Public scripting API"
  (:require [wish-engine.runtime.api :refer-macros [defn-api]]
            [wish-engine.util :as util :refer [conj-vec throw-msg]]))


(def exported-fns {})

(def ^:dynamic *engine-state* nil)
(def ^:dynamic *apply-context* nil)
(def ^{:dynamic true
       :private true}
  *list-inflation-context* nil)


; ======= utils ===========================================

(defn throw-arg
  ([fn-name arg] (throw-arg fn-name arg nil))
  ([fn-name arg reason-str]
   (let [reason (when reason-str
                  (str " (" reason-str ")"))]
     (throw-msg fn-name ": Invalid argument" reason ":\n" arg))))

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

(def ^:private key-or-map? #(or (keyword? %)
                                (map? %)))

(defn- ->map [entities]
  (reduce (fn [m f]
            (assoc m (:id f) f))
          {}
          entities))

; ======= Validation ======================================

(defn validate-feature-map [m]
  (letfn [(throw-reason [& args]
            (throw-msg "Invalid feature ("
                       (apply str args)
                       "):\n"
                       m))]
    (when-not (:id m)
      (throw-reason "missing :id"))

    ; return if valid
    m))

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

(defn compile-feature-map [m]
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
                               (compile-feature-map option)

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
              (map validate-feature-map)
              (map compile-feature-map)

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
  (declare-toplevel "declare-race" [:subraces parent-race-id] [race-spec]))


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

  (let [attr-path (cond
                    (vector? attr-id-or-path) attr-id-or-path
                    (keyword? attr-id-or-path) [attr-id-or-path]
                    :else (throw-arg "provide-attr" attr-id-or-path
                                     ":attr or [:attr :path]"))

        state (assoc-in state (cons :attrs attr-path) value)]

    (if-let [context *apply-context*]
      (assoc-in state
                (cons :attrs/meta attr-path)
                {:wish-engine/source context})
      state)))

;;;
;;; Provide Feature
;;;

(defn-api provide-feature [state feature]
  (when *engine-state*
    (throw-msg "provide-feature(s) must not be called at the top level. Try `declare-features`"))

  (let [id (cond
             (map? feature) (:id feature)
             (keyword? feature) feature
             :else (throw-arg "provide-feature" feature
                              "feature ID or map"))
        declared-inline? (map? feature)

        feature (if declared-inline?
                  (->> feature
                       validate-feature-map
                       compile-feature-map)
                  (util/feature-by-id state id))

        ; prepare for instancing
        instanced? (:instanced? feature)
        next-instance (get-in state [:wish/instances id] 0)
        with-instance (if-not instanced? feature
                        (assoc instance
                         :wish/instance next-instance
                         :wish/instance-id (util/instance-id
                                             (:id feature)
                                             (:id state)
                                             next-instance)))]

    (when (and (not instanced?)
               (> next-instance 0))
      (js/console.warn "Adding duplicate feature " id
                       " from " *apply-context*
                       " but it is not instanced"))

    (as-> state state

      ; declare the feature on the entity state, if a map
      (if-not declared-inline? state
        (assoc-in state [:declared-features id] feature))

      (update-in state [:wish/instances id] inc)

      ; install the feature
      (update state :active-features conj-vec
              (as-> {:id id} instance

                ; attach the context
                (if-let [ctx *apply-context*]
                  (assoc instance :wish-engine/source ctx)
                  instance)

                ; attach instance info
                (if-not instanced? instance
                  (merge (select-keys with-instance [:wish/instance
                                                     :wish/instance-id])
                         instance))))

      ; apply any apply-fn
      (if-let [apply-fn (:! feature)]
        (apply-fn state)
        state)

      ; apply options
      (if-let [selected-options (util/selected-options state with-instance)]
        (reduce
          (fn [s option]
            (if-let [apply-fn (:! option)]
              (apply-fn s)
              s))
          state
          selected-options)
        state))))

(defn-api provide-features [state & features]
  (loop [state state
         features (flatten-lists
                    "provide-features"
                    key-or-map?
                    features)]
    (if-let [f (first features)]
      (recur (provide-feature state f)
             (rest features))
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
                         (if (sequential? results)
                           results
                           [results])))))))

(defn-api by-id
  "Given an ID, returns a function of `state` that will fetch
   the feature (or list item) with that ID"
  [id]
  (when-not (keyword? id)
    (throw-arg "by-id" id
               "entity id keyword"))

  (fn entity-by-id [state]
    (or (get-in state [:list-entities id])
        (get-in state [:features id])
        (js/console.warn "Could not find entity with ID " id))))

(defn-api items-from-list
  "Given a list ID, returns a function of `state` that will fetch
   the items from that list"
  [list-id]
  (when-not (keyword? list-id)
    (throw-arg "items-from-list" list-id
               "list id keyword"))

  (fn list-items-by-id [state]
    (or (inflate-list state list-id)
        (js/console.warn "Could not find list with ID " list-id))))

(defn-api options-of
  "Given a feature ID, returns a function of `state` that fetches the
   selected options of that feature"
  [feature-id]
  (when-not (keyword? feature-id)
    (throw-arg "options-of" feature-id
               "list id keyword"))

  (fn feature-options-by-id [state]
    (if-let [f (util/feature-by-id state feature-id)]
      (util/selected-options state f)
      (js/console.warn "Could not find feature: " feature-id))))

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
                            (map (comp compile-feature-map validate-feature-map)))
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
