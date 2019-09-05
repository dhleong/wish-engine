(ns wish-test.advanced-test
  "Testing for advanced compilation mode support"
  (:require [cljs.test :refer-macros [deftest testing is]]))

(defn advanced-eval [s]
  (js/wish_test_helper.engine.eval_string s))

(deftest advanced-test
  (testing "Basic math"
    (is (= 42 (advanced-eval "(+ 20 22)")))))
