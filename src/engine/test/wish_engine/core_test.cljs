(ns wish-engine.core-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [wish-engine.test-util :refer [eval-form]]))

(deftest engine-test
  (testing "Basic compilation"
    (is (= 42 (eval-form '(+ 20 22)))))

  (testing "Exported functions"
    (is (= "1st" (eval-form '(ordinal 1))))
    (is (= true (eval-form '(has? #{:mreynolds} [:mreynolds])))))

  (testing "Macro evaluation"
    (is (= 9001 (eval-form '(when 42 9001))))

    (let [f (eval-form '(fn [v]
                          (cond
                            (<= 42 v 9001) :firefly
                            (> v 9001) :serenity
                            :else :alliance)))]
      (is (= :firefly (f 42)))
      (is (= :serenity (f 9002)))
      (is (= :alliance (f 0))))))
