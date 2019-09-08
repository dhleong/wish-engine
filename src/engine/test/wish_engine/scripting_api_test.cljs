(ns wish-engine.scripting-api-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [wish-engine.util :refer [eval-form]]))

(deftest utils-test
  (testing "Ordinal"
    (is (= "1st" (eval-form '(ordinal 1))))
    (is (= "2nd" (eval-form '(ordinal 2))))))

