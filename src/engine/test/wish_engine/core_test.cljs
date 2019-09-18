(ns wish-engine.core-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [wish-engine.test-util :refer [eval-state eval-state]]
            [wish-engine.core :as core]
            [wish-engine.scripting-api :as api]))

(deftest load-source-test
  (testing "load-source with quoted form"
    (let [e (core/create-engine)
          s (core/create-state e)
          _ (core/load-source e s
                              (str "(declare-features "
                                   "  {:id :eq "
                                   "   :5e/starting-eq "
                                   "   #'[(:mace :warhammer)]})"))
          loaded (get-in @s [:features :eq])]
      (is (some? loaded))
      (is (list? (get-in loaded [:5e/starting-eq 0]))))))

(deftest inflate-entity-test
  (testing "Inflate entity"
    (let [{{f :base-feature} :features :as state}
          (eval-state '(declare-features
                         {:id :serenity
                          :! (on-state
                               (provide-attr
                                 :serenity
                                 true))}

                         {:id :base-feature
                          :! (on-state
                               (provide-attr
                                 :base-feature
                                 true)
                               (provide-features
                                 :serenity

                                 {:id :provided-feature
                                  :! (on-state
                                       (provide-attr
                                         :provided-feature
                                         true)

                                       (provide-features
                                         {:id :three-nested
                                          :! (on-state
                                               (provide-attr
                                                 :three-nested
                                                 true))}))}))}))
          inflated (core/inflate-entity
                     state
                     f
                     {}
                     {})]

      (is (= {:base-feature true
              :serenity true
              :provided-feature true
              :three-nested true}
             (:attrs inflated)))
      (is (= [:serenity
              :provided-feature
              :three-nested]
             (->> inflated
                  :active-features
                  (map :id))))
      (is (= [:serenity
              :provided-feature
              :three-nested]
             (->> inflated
                  :sorted-features
                  (map :id))))))

  (testing "Implicit feature support"
    (let [state (eval-state
                  '(declare-list
                     :ships
                     {:id :firefly})
                  '(declare-class
                     {:id :captain
                      :levels {3 {:! (on-state
                                       (provide-feature
                                         {:id :piloting
                                          :values (options-of
                                                    :captain/ship)}))}}}))
          options {:captain/ship [:firefly]
                   :piloting [:firefly]}
          captain (core/inflate-class
                    state
                    :captain
                    {:level 3}
                    options)]
      (is (= [{:id :firefly}]
             (-> captain :features :piloting :wish-engine/selected-options)))))

  (testing "Instanced feature inflation"
    (let [state (eval-state
                  '(declare-list
                     {:id :all-weapons
                      :type :feature}
                     {:id :pistol
                      :! (on-state
                           (provide-attr :pistol true))}
                     {:id :rifle
                      :! (on-state
                           (provide-attr :rifle true))}
                     {:id :vera
                      :! (on-state
                           (provide-attr :rifle true))})
                  '(declare-class
                     {:id :captain
                      :! (on-state
                           (provide-features
                             {:id :sidearm
                              :instanced? true
                              :max-options (if (< (:level state) 6)
                                             2
                                             4)
                              :value (items-from-list :all-weapons)}))}))
          options {:sidearm#captain#0 {:id :sidearm
                                       :value [:pistol :rifle]}}
          captain (core/inflate-class
                    state
                    :captain
                    {:level 3}
                    options)]
      (is (= {:pistol true
              :rifle true}
             (-> captain :attrs))))))

(deftest inflate-entities-test
  (testing "Inflate keywords"
    (let [state (eval-state '(declare-list
                               {:id :ships
                                :type :features}
                               {:id :firefly}
                               {:id :komodo}
                               {:id :peregrin}))]
      (is (= [{:id :peregrin}
              {:id :firefly}]
             (core/inflate-entities
               state
               [:peregrin
                :firefly])))))

  (testing "Inflate fn calls"
    (let [state (eval-state '(declare-list
                               {:id :ships
                                :type :features}
                               {:id :firefly}
                               {:id :komodo}
                               {:id :peregrin}))]
      (is (= [{:id :firefly}
              {:id :komodo}
              {:id :peregrin}
              {:id :firefly}]
             (core/inflate-entities
               state
               [(api/items-from-list :ships)
                (api/by-id :firefly)]))))))

(deftest inflate-race-test
  (testing "Inflate subrace"
    (let [state (eval-state
                  '(declare-race
                     {:id :human
                      :! (on-state
                           (provide-attr :human? true))
                      :levels {3 {:! (on-state
                                      (provide-attr :aims-to-misbehave? true))}}})
                  '(declare-subrace
                     :human
                     {:id :human/captain
                      :levels {3 {:! (on-state
                                       (provide-attr
                                         :super-captain?
                                         true))}}}))
          inflated (core/inflate-race
                     state
                     :human/captain
                     {:level 3}
                     {})]
      (is (= {:human? true
              :aims-to-misbehave? true
              :super-captain? true}
             (:attrs inflated))))))
