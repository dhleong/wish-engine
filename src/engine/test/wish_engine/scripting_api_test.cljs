(ns wish-engine.scripting-api-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [wish-engine.util :refer [eval-form eval-state]]))

(deftest utils-test
  (testing "Ordinal"
    (is (= "1st" (eval-form '(ordinal 1))))
    (is (= "2nd" (eval-form '(ordinal 2))))))

(deftest declare-features-test
  (testing "Add features to state"
    (let [state (eval-state '(declare-features
                               {:id :serenity}))]
      (is (= {:features {:serenity {:id :serenity}}}
             state))))

  (testing "Create functions from :!"
    (let [state (eval-state '(declare-features
                               {:id :serenity
                                :! (fn [state]
                                     (assoc state :ran? true))}))
          feature (get-in state [:features :serenity])
          apply-fn (:! feature)]
      (is (ifn? apply-fn))
      (is (= {:wish-engine/source :serenity}
             (meta apply-fn)))
      (is (= {:ran? true}
             (apply-fn {}))))))

(deftest level-scaling-test
  (testing "Levels only"
    (let [{{f :serenity} :features} (eval-state
                                      '(declare-features
                                         {:id :serenity
                                          :levels {2 {:! (on-state
                                                           (assoc :two? true))}
                                                   3 {:! (on-state
                                                           (assoc :three? true))}
                                                   4 {:! (on-state
                                                           (assoc :two? "no-longer"
                                                                  :four? true))}}}))
          state!-base (:! f)
          state! (comp #(dissoc % :level) state!-base)]
      (is (= {:wish-engine/source :serenity}
             (meta state!-base)))

      (is (empty? (state! {})))
      (is (empty? (state! {:level 1})))

      (is (= {:two? true}
             (state! {:level 2})))

      ; levels are additive
      (is (= {:two? true
              :three? true}
             (state! {:level 3})))

      ; levels are applied in order
      (is (= {:two? "no-longer"
              :three? true
              :four? true}
             (state! {:level 4}))))))

(deftest provide-attr-test
  (testing "Provide attrs from :! fn"
    (let [{{f :serenity} :features} (eval-state
                                      '(declare-features
                                         {:id :serenity
                                          :! (on-state
                                               (provide-attr
                                                 [:ship :role]
                                                 :captain))}))
          state! (:! f)]
      (is (ifn? state!))
      (is (= {:attrs {:ship {:role :captain}}
              :attrs/meta {:ship {:role {:wish-engine/source :serenity}}}}
             (state! {}))))))

(deftest provide-features-test
  (testing "Provide feature from :! fn"
    (let [{{f :serenity} :features} (eval-state
                                      '(declare-features
                                         {:id :serenity
                                          :! (on-state
                                               (provide-features
                                                 :rank/captain

                                                 {:id :captain/sidearm}))}))
          state! (:! f)]
      (is (ifn? state!))
      (is (= {:active-features {:rank/captain {:wish-engine/source :serenity}
                                :captain/sidearm {:wish-engine/source :serenity}}
              :declared-features {:captain/sidearm {:id :captain/sidearm}}}
             (state! {}))))))
