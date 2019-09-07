(ns wish-engine.runtime-eval-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [wish-engine.util :refer [eval-form]]))

(deftest basic-test
  (testing "let"
    (is (= 42 (eval-form '(let [n 21]
                            (* 2 n)))))))

;;;
;;; Macro expansion
;;;

(deftest threading-test
  (testing "->"
    (is (= 42 (eval-form '(-> 10
                              (* 2)
                              (+ 1)
                              (* 2)))))

    ;; FIXME actually verify the correct placement
    )

  (testing "->>"
    (is (= 42 (eval-form '(-> 10
                              (* 2)
                              (+ 1)
                              (* 2)))))

    ;; FIXME actually verify the correct placement
    )

  (testing "as->"
    (is (= 42 (eval-form '(as-> (* 10 2) n
                            (* 2 n)
                            (+ 2 n)))))))

(deftest cond-forms-test
  (testing "cond"
    (is (= :firefly (eval-form '(cond
                                  (<= 42 9001) :firefly
                                  (> v 9001) :serenity
                                  :else :alliance))))
    (is (= :serenity (eval-form '(cond
                                   (>= 42 9001) :firefly
                                   (< 42 9001) :serenity
                                   :else :alliance))))
    (is (= :alliance (eval-form '(cond
                                   (>= 42 9001) :firefly
                                   (= 42 9001) :serenity
                                   :else :alliance))))))

(deftest if-forms-test
  (testing "if-let"
    (is (= 42 (eval-form '(if-let [n 21]
                            (* 2 n)
                            9001))))
    (is (= 9001 (eval-form '(if-let [n false]
                              (* 2 n)
                              9001)))))

  (testing "if-not"
    (is (= "true" (eval-form '(if-not true
                                "false"
                                "true"))))))

