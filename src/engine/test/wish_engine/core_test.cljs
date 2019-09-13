(ns wish-engine.core-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [wish-engine.test-util :refer [eval-state]]
            [wish-engine.core :as core]))

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
                  :features
                  (map :id)))))))

