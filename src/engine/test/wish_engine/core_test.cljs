(ns wish-engine.core-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [wish-engine.core :refer [create-engine]]
            [wish-engine.model :as engine]))

(defn- eval-form [form]
  (let [eng (create-engine)
        state (engine/create-state eng)]
    (engine/eval-source-form eng state form)))

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
                            (<= 42 v 9001) :serenity
                            (< v 42) :firefly
                            :else :alliance)))]
      (is (= :serenity (f 42)))
      (is (= :firefly (f 41)))
      (is (= :alliance (f 0))))))
