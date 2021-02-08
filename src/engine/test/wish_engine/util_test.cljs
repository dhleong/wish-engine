(ns wish-engine.util-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [wish-engine.util :refer [merge-entities]]))

(deftest merge-entities-test
  (testing "Non-heterogeneous merge"
    (is (= {:name "serenity"
            :conflict [{:id :ship} {:id :captain}]}
           (merge-entities
             {:name "serenity"
              :conflict {:id :crew}}
             {:conflict [{:id :ship} {:id :captain}]})))))
