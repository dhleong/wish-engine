(ns wish-test.advanced-test
  "Testing for advanced compilation mode support"
  (:require [cljs.test :refer-macros [deftest testing is]]))

(defn advanced-eval [s]
  (println s)
  nil
  #_(js/wish_test_helper.engine.eval_string s))

(deftest advanced-test
  (testing "Basic math"
    (println "math")
    (is (= 42 (advanced-eval "(+ 20 22)"))))

  (testing "Fn compilation"
    (let [f (advanced-eval "(fn [v]
                              (+ 20 v))")]
      (println "compile fn")
      (is (ifn? f))
      (is (= 42 (f 22)))))

  (testing "Cond macro"
    (let [f (advanced-eval "(fn [v]
                              (cond
                                (< v 9001) :serenity
                                :else :alliance))")]
      (is (ifn? f))
      (is (= :serenity (f 42))))))
