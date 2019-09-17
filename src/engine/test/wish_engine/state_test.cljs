(ns wish-engine.state-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [wish-engine.test-util :refer [eval-state-ref]]
            [wish-engine.state :as state]))

(deftest state-test
  (testing "Combine states"
    (let [a (eval-state-ref
              '(declare-features
                 {:id :captain}))
          b (eval-state-ref
              '(declare-features
                 {:id :ship}))
          combined (state/plus a b)]

      (is (= #{:captain :ship}
             (->> @combined
                  :features
                  keys
                  (into #{}))))))

  (testing "Composite states"
    (let [a (eval-state-ref
              '(declare-features
                 {:id :captain}))
          b (eval-state-ref
              '(declare-features
                 {:id :ship}))
          c (eval-state-ref
              '(declare-features
                 {:id :crew}))
          combined (state/composite a b c)]

      (is (= #{:captain :crew :ship}
             (->> @combined
                  :features
                  keys
                  (into #{}))))))

  (testing "State equality"
    (let [a (eval-state-ref
              '(declare-features
                 {:id :captain}))
          b (eval-state-ref
              '(declare-features
                 {:id :captain}))]

      (is (= a b))
      (is (= (hash a) (hash b))))))

