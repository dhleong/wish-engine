(ns wish-engine.core-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [wish-engine.core :refer [create-engine]]))

(deftest engine-test
  (testing "Feature declaration"
    (let [eng (create-engine)]
      (is (nil? eng)))))
