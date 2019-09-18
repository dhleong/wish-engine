(ns wish-engine.state
  (:require [wish-engine.util :as util]))

(defprotocol IEngineState
  (plus
    [this ^IEngineState other]
    "Combine another IEngineState instance with this one
     into a new IEngineState"))

(deftype SimpleEngineState [state]
  #?@(:clj [Object
            (equals [this other]
                    (or (identical? this other)
                        (when other
                          (= @state @(.-state other)))))
            (hashCode [this]
                      (hash @state))]

      :cljs [Object
             (equiv [this other]
                    (-equiv this other))

             IEquiv
             (-equiv [this other]
                     (or (identical? this other)
                         (when other
                           (= @state @(.-state other)))))

             IHash
             (-hash [this] (hash @state))])


  IEngineState
  (plus [this ^IEngineState other]
    (SimpleEngineState. (atom (util/merge-entities
                                @state
                                @(.-state other)))))

  #?@(:clj [clojure.lang.IDeref
            (deref [this] @state)]
      :cljs [IDeref
             (-deref [this] @state)])

  #?@(:clj [clojure.lang.IAtom
            (swap [this f]
                  (.swap state f))
            (swap [this f arg]
                  (.swap state f arg))
            (swap [this f arg1 arg2]
                  (.swap state f arg1 arg2))
            (swap [this f x y args]
                  (.swap state f x y args))
            (compareAndSet [this oldv newv]
                           (.compareAndSet state oldv newv))
            (reset [this newval]
                   (.reset state newval))]

      :cljs [IAtom
             ISwap
             (-swap! [this f] (swap! state f))
             (-swap! [this f x] (swap! state f x))
             (-swap! [this f x y] (swap! state f x y))
             (-swap! [this f x y more] (apply swap! state f x y more))

             IReset
             (-reset! [this new-value] (-reset! state new-value))]))

(defn create [initial-data]
  (->SimpleEngineState (atom initial-data)))

(defn value [engine-state]
  (if (map? engine-state)
    engine-state
    @engine-state))

(defn with-entity
  ([state entity] (with-entity state entity nil))
  ([state entity options]
   (assoc entity
          :wish-engine/state (value state)
          :wish-engine/options options)))

(defn clean-entity [entity]
  (dissoc entity :wish-engine/state :wish-engine/options))

(defn composite
  "Given multiple IEngineStates, produce a single IEngineState
   that is a composite of them all"
  [& states]
  (reduce plus states))
