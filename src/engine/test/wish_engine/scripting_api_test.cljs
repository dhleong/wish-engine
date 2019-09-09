(ns wish-engine.scripting-api-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [wish-engine.test-util :refer [eval-form eval-state]]
            [wish-engine.scripting-api :as api]))

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

(deftest declare-items-test
  (testing "Merge item item defs"
    (let [state (eval-state '(declare-items
                               {:type :weapon
                                :attrs {:subtype :gun}
                                :ammunition [1 2]}

                               {:id :gun/pistol
                                :attrs {:laser? true}
                                :ammunition [3 4]}))]
      (is (= {:id :gun/pistol
              :ammunition [1 2 3 4]
              :attrs {:subtype :gun
                      :laser? true}
              :type :weapon}

             (get-in state [:items :gun/pistol]))))))

(deftest declare-options-test
  (testing "Add options to state"
    (let [state (eval-state '(declare-options
                               :ship
                               {:id :serenity}))]
      (is (= {:options {:ship {:serenity {:id :serenity}}}}
             state)))))

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

(deftest limited-use-test
  (testing "Provide default :restore-amount"
    (let [{{f :serenity} :features} (eval-state
                                      '(declare-features
                                         {:id :serenity
                                          :! (on-state
                                               (add-limited-use
                                                 {:id :fuel#uses
                                                  :name "Fuel"}))}))
          state! (:! f)
          s (state! {})
          limited-use (get-in s [:limited-uses :fuel#uses])]
      (is (ifn? (:restore-amount limited-use)))
      (is (= 42 ((:restore-amount limited-use) {:used 42})))))

  (testing "Support constant :restore-amount"
    (let [{{f :serenity} :features} (eval-state
                                      '(declare-features
                                         {:id :serenity
                                          :! (on-state
                                               (add-limited-use
                                                 {:id :fuel#uses
                                                  :name "Fuel"
                                                  :restore-amount 2}))}))
          state! (:! f)
          s (state! {})
          limited-use (get-in s [:limited-uses :fuel#uses])]
      (is (ifn? (:restore-amount limited-use)))
      (is (= 2 ((:restore-amount limited-use) {:used 42}))))))

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

                                                 {:id :captain/sidearm
                                                  :name "Sidearm"}))}))
          state! (:! f)]
      (is (ifn? state!))
      (is (= {:active-features [{:id :rank/captain
                                 :wish-engine/source :serenity}
                                {:id :captain/sidearm
                                 :wish-engine/source :serenity}]
              :declared-features {:captain/sidearm {:id :captain/sidearm
                                                    :name "Sidearm"}}}
             (state! {})))))

  (testing "Apply inline options"
    (let [{{f :serenity} :features} (eval-state
                                      '(declare-features
                                         {:id :serenity
                                          :! (on-state
                                               (provide-features
                                                 {:id :captain/sidearm
                                                  :name "Sidearm"
                                                  :values
                                                  [{:id :pistol
                                                    :! (on-state
                                                         (provide-attr
                                                           [:sidearm :pistol]
                                                           true))}]}))}))
          state! (:! f)]
      (is (ifn? state!))
      (is (empty? (:attrs (state! {}))))

      (let [inflated (state! {:wish-engine/options
                              {:captain/sidearm [:pistol]}})]
        (is (= {:sidearm {:pistol true}}
               (:attrs inflated)))
        (is (= {:sidearm {:pistol {:wish-engine/source :pistol}}}
               (:attrs/meta inflated)))))))


; ======= list handling ===================================

(deftest add-to-list-test
  (testing "Simple list creation"
    (let [state (eval-state '(add-to-list
                               :crew
                               [{:id :mreynolds}
                                {:id :zoe}
                                {:id :itskaylee}]))]
      (is (= [{:id :mreynolds}
              {:id :zoe}
              {:id :itskaylee}]
             (api/inflate-list state :crew)))))

  (testing "add items by id"
    (let [state (eval-state '(do
                               ; list entity
                               (add-to-list
                                 :people
                                 [{:id :mreynolds}])

                               ; feature list
                               (add-to-list
                                 {:id :soldiers
                                  :type :feature}
                                 [{:id :zoe}])

                               ; declared feature
                               (declare-features
                                 {:id :mechanic/itskaylee})

                               (add-to-list
                                :crew
                                [(by-id :mreynolds)
                                 (by-id :zoe)
                                 (by-id :mechanic/itskaylee)])))]
      (is (= [{:id :mreynolds}
              {:id :zoe}
              {:id :mechanic/itskaylee}]
             (api/inflate-list state :crew)))))

  (testing "add items from list by id"
    (let [state (eval-state '(do
                               ; list entity
                               (add-to-list
                                 :people
                                 [{:id :mreynolds}
                                  {:id :zoe}
                                  {:id :itskaylee}])

                               (add-to-list
                                :crew
                                [(items-from-list :people)])))]
      (is (= [{:id :mreynolds}
              {:id :zoe}
              {:id :itskaylee}]
             (api/inflate-list state :crew)))))

  (testing "add items from feature option")
  )
