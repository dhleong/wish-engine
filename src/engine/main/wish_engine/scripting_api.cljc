(ns wish-engine.scripting-api
  "Public scripting API"
  (:require [wish-engine.runtime.api :refer-macros [defn-api]]
            [wish-engine.util :refer [conj-vec feature-by-id throw-msg]]))


(def exported-fns {})

(def ^:dynamic *engine-state* nil)
(def ^:dynamic *apply-context* nil)


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
              (valid? f) [f]
              (seq? f) f
              :else (throw-arg fn-name f)))
          args))

(def ^:private key-or-map? #(or (keyword? %)
                                (map? %)))


; ======= Validation ======================================

(defn validate-feature-map [m]
  (letfn [(throw-reason [& args]
            (throw-msg "Invalid feature ("
                       (apply str args)
                       "):\n"
                       m))]
    (when-not (:id m)
      (throw-reason "missing :id"))

    ; return m if valid
    m))


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

(defn-api declare-features [& features]
  (when-not *engine-state*
    (throw-msg "declare-features must be called at the top level"))

  (swap! *engine-state*
         update
         :features
         merge

         (->> features

              (flatten-lists "declare-features" map?)
              (map validate-feature-map)
              (map compile-feature-map)

              (reduce (fn [m f]
                        (assoc m (:id f) f))
                      {}))))


; ======= Entity-modifying forms ==========================

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
      (assoc-in state (cons :attrs/meta attr-path) {:wish-engine/source context})
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
                              "feature ID or map"))]

    (as-> state state

      ; declare the feature on the entity state, if a map
      (if-not (map? feature) state
        (->> feature
             validate-feature-map
             compile-feature-map
             (assoc-in state [:declared-features id])))

      ; install the feature
      (update state :active-features conj-vec
              (let [base {:id id}]
                (if-let [ctx *apply-context*]
                  (assoc base :wish-engine/source ctx)
                  base)))

      ; apply any apply-fn
      (if-let [apply-fn (if (map? feature)
                          (:! feature)
                          (:! (feature-by-id state id)))]
        (apply-fn state)
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
