(ns wish-engine.core-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [wish-engine.test-util :refer [eval-state]]
            [wish-engine.core :as core]))

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

