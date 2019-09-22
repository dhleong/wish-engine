(ns wish-test.advanced-test
  "Testing for advanced compilation mode support"
  (:require [cljs.test :refer-macros [deftest testing is]]))

(defn advanced-eval [s]
  (js/wish_test_helper.engine.eval_string s))

(deftest advanced-test
  (testing "Basic math, fn interpolation"
    (is (= 42 (advanced-eval "(+ 20 22)")))
    (is (= true (advanced-eval "(not false)")))
    (is (nil? (advanced-eval "(if (not true) 42)"))))

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

(deftest destructuring-test
  (testing "fn arg destructure"
    ;; NOTE: due the way our advanced tests are run (as mentioned above)
    ;; we cannot directly call fns with maps created in "our" context, so
    ;; we test destructuring within the bootstrapped context
    (is (= "mreynolds"
           (advanced-eval "((fn [#{id}] id) {:id \"mreynolds\"})")))

    (is (= 42
           (advanced-eval
             (str "((fn [#{modifiers}] (+ 10 (:str modifiers))) "
                  "{:modifiers {:str 32}})"))))))

(deftest api-test
  (testing "Scripting API"
    (is (= "1st" (advanced-eval "(ordinal 1)")))
    (is (= "3rd" (advanced-eval "(ordinal 3)")))))
