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
          feature (get-in state [:features :serenity])]
      (is (ifn? (:! feature)))
      (is (= {:ran? true}
             ((:! feature) {}))))))
