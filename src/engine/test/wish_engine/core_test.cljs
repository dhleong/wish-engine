(ns wish-engine.core-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [wish-engine.test-util :refer [eval-state eval-state]]
            [wish-engine.core :as core]
            [wish-engine.scripting-api :as api]))

(deftest load-source-test
  (testing "load-source with quoted form"
    ; FIXME there's no obvious way to properly support (quote) like this using
    ; our current postwalk-based compilation, since it will see the `(:mace
    ; :warhammer)` form and, having no way of knowing it should be quoted,
    ; convert it to `(get :warhammer :mace)`. Maybe we can do something
    ; fancy with specter...
    ; Similarly, our unknown-fn-call routine has no way of knowing whether the
    ; symbol has been declared by a `let` binding or something, but at least
    ; that won't break anything....
    (let [e (core/create-engine)
          s (core/create-state e)
          _ (core/load-source e s
                              (str "(declare-features "
                                   "  {:id :eq "
                                   "   :5e/starting-eq "
                                   "   #'[(:mace :warhammer)]})"))
          loaded (get-in @s [:features :eq])]
      (is (some? loaded))
      #_(is (list? (get-in loaded [:5e/starting-eq 0]))))))

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
                  (map :id)))))))

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
