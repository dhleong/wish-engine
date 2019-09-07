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
    ;; TODO
    ))

(deftest if-forms-test
  (testing "if-let"
    (is (= 42 (eval-form '(if-let [n 21]
                            (* 2 n)
                            9001))))
    (is (= 9001 (eval-form '(if-let [n false]
                              (* 2 n)
                              9001))))))

