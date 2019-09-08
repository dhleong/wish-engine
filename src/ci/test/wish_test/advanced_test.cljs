(ns wish-test.advanced-test
  "Testing for advanced compilation mode support"
  (:require [cljs.test :refer-macros [deftest testing is]]))

(defn advanced-eval [s]
  (js/wish_test_helper.engine.eval_string s))

(deftest advanced-test
  (testing "Basic math, fn interpolation"
    (is (= 42 (advanced-eval "(+ 20 22)")))
    (is (= true (advanced-eval "(not false)")))
    #_(is (nil? (advanced-eval "(if (not true) 42)"))))

  (testing "Macro expansion"
    (is (= 42 (advanced-eval "(when true 42)")))
    (is (nil? (advanced-eval "(when-not true 42)"))))

  (testing "Fn compilation"
    (let [f (advanced-eval "(fn [v]
                              (+ 20 v))")]
      (is (ifn? f))
      (is (= 42 (f 22)))))

  (testing "Cond macro"
    (let [f (advanced-eval "(fn [v]
                              (cond
                                (>= v 9001) :serenity
                                :else :alliance))")]
      (is (ifn? f))

      ; NOTE: when running this test, the Keyword function is not shared with
      ; the bootstrapped context, so they don't directly equal
      (is (= (str :serenity)
             (str (f 9001))))
      (is (= (str :alliance)
             (str (f 0)))))))
