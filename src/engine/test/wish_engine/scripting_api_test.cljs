(ns wish-engine.scripting-api-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [wish-engine.test-util :refer [eval-form eval-state]]
            [wish-engine.core :as core]
            [wish-engine.scripting-api :as api]
            [wish-engine.state :as state]))

(defn- ->ids [entities]
  (map :id entities))

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
    (let [{{f :serenity} :features :as state}
          (eval-state
            '(declare-features
               {:id :rank/captain}
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
             (-> (state! {:wish-engine/state state})
                 (select-keys [:active-features :declared-features]))))))

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
               (:attrs/meta inflated))))))

  (testing "Support using items-from-list for feature options"
    (let [{{f :crew-member} :classes :as state}
          (eval-state
            '(declare-features
               {:id :weapon
                :values (items-from-list :weapons)
                :! (on-state
                     (provide-features
                       {:id :weapon/spare
                        :values (items-from-list :weapons)}))})

            '(declare-list
               :weapons
               {:id :knife
                :! (on-state (provide-attr :knife true))}
               {:id :pistol
                :! (on-state (provide-attr :pistol true))}
               {:id :rifle
                :! (on-state (provide-attr :rifle true))}
               {:id :vera
                :! (on-state (provide-attr :vera true))})

            '(declare-class
               {:id :crew-member
                :! (on-state
                     (provide-features :weapon))}))
          state! (:! f)]
      (is (ifn? state!))
      (is (empty? (:attrs (state! {:wish-engine/state state}))))

      (let [inflated (state! {:wish-engine/options
                              {:weapon [:pistol]}
                              :wish-engine/state state})]
        (is (= {:pistol true}
               (:attrs inflated))))
      (let [inflated (state! {:wish-engine/options
                              {:weapon/spare [:vera]}
                              :wish-engine/state state})]
        (is (= {:vera true}
               (:attrs inflated)))))))


; ======= instancing ======================================

(deftest instancing-test
  (testing "Expand instanced features"
    (let [{{c :crew-member} :classes :as state}
          (eval-state '(do
                         (declare-features
                           {:id :weapon
                            :instanced? true
                            :max-options 1})

                         (declare-options
                           :weapon
                           {:id :knife
                            :! (on-state (provide-attr :knife true))}
                           {:id :pistol
                            :! (on-state (provide-attr :pistol true))}
                           {:id :rifle
                            :! (on-state (provide-attr :rifle true))}
                           {:id :vera
                            :! (on-state (provide-attr :vera true))})

                         (declare-class
                           {:id :crew-member
                            :! (on-state
                                 (provide-features :weapon))
                            :levels {2 {:! (on-state
                                             (provide-features :weapon))}
                                     3 {:! (on-state
                                             (provide-features :weapon))}
                                     4 {:! (on-state
                                             (provide-features :weapon))}}})))
          inflated (core/inflate-entity
                     state
                     c
                     {:id (:id c)
                      :level 4}
                     {:weapon#crew-member#0 #{:knife}
                      :weapon#crew-member#1 #{:pistol}
                      :weapon#crew-member#2 #{:rifle}
                      :weapon#crew-member#3 #{:vera}})]

      (is (= [:weapon#crew-member#0
              :weapon#crew-member#1
              :weapon#crew-member#2
              :weapon#crew-member#3]

             (->> inflated
                  :sorted-features
                  (map :wish/instance-id))))

      ; options were applied
      (is (= {:knife true
              :pistol true
              :rifle true
              :vera true}
             (:attrs inflated)))

      ; options are attached
      (is (= [:knife]
             (-> inflated :sorted-features first
                 :wish-engine/selected-options
                 ->ids)))))

  (testing "Provide instance-id with state map"
    (let [{{c :crew-member} :classes :as state}
          (eval-state '(do
                         (declare-features
                           {:id :weapon
                            :instanced? true
                            :max-options 1})

                         (declare-options
                           :weapon
                           {:id :pistol
                            :! (on-state
                                 (provide-attr
                                   [:weapons (:wish/instance-id state)]
                                   :pistol))}
                           {:id :vera
                            :! (on-state
                                 (provide-attr
                                   [:weapons (:wish/instance-id state)]
                                   :vera))})

                         (declare-class
                           {:id :crew-member
                            :! (on-state
                                 (provide-features :weapon))
                            :levels {2 {:! (on-state
                                             (provide-features :weapon))}}})))
          inflated (core/inflate-entity
                     state
                     c
                     {:id (:id c)
                      :level 4}
                     {:weapon#crew-member#0 #{:pistol}
                      :weapon#crew-member#1 #{:vera}})]

      ; options were applied
      (is (= {:weapons {:weapon#crew-member#0 :pistol
                        :weapon#crew-member#1 :vera}}
             (:attrs inflated)))))
  )


; ======= list handling ===================================

(deftest add-to-list-test
  (testing "Simple list creation"
    (let [state (eval-state '(declare-list
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
                               (declare-list
                                 :people
                                 {:id :mreynolds})

                               ; feature list
                               (declare-list
                                 {:id :soldiers
                                  :type :feature}
                                 [{:id :zoe}])

                               ; declared feature
                               (declare-features
                                 {:id :mechanic/itskaylee})

                               (declare-list
                                :crew
                                [(by-id :mreynolds)
                                 (by-id :zoe)
                                 (by-id :mechanic/itskaylee)
                                 (by-id :wishywash)])))]
      (is (= [{:id :mreynolds}
              {:id :zoe}
              {:id :mechanic/itskaylee}
              {:id :wishywash}]
             (api/inflate-list
               (assoc-in state [:list-entities :wishywash]
                         {:id :wishywash})
               :crew)))

      (is (= [{:id :mreynolds}
              {:id :zoe}
              {:id :mechanic/itskaylee}
              {:id :wishywash}]
             (api/inflate-list
               (state/with-entity
                 state
                 {:id :serenity
                  :list-entities {:wishywash {:id :wishywash}}})
               :crew)))))

  (testing "add items from list by id"
    (let [state (eval-state '(do
                               ; list entity
                               (declare-list
                                 :people
                                 {:id :mreynolds}
                                 {:id :zoe}
                                 {:id :itskaylee})

                               (declare-list
                                :crew
                                (items-from-list :people))))]
      (is (= [{:id :mreynolds}
              {:id :zoe}
              {:id :itskaylee}]
             (api/inflate-list state :crew)))))

  (testing "add items from feature option"
    (let [state (eval-state '(do
                               ; list entity
                               (declare-features
                                 {:id :people
                                  :values [{:id :mreynolds}
                                           {:id :zoe}]})

                               (declare-options
                                 :people
                                 {:id :itskaylee})

                               (declare-list
                                :crew
                                (options-of :people))))]
      (is (= [{:id :itskaylee}
              {:id :zoe}
              {:id :mreynolds}]
             (api/inflate-list
               {:wish-engine/options {:people [:itskaylee :zoe :mreynolds]}
                :wish-engine/state state}
               :crew))))))
