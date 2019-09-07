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
                                "true")))))

  (testing "if-some"
    (is (= "false" (eval-form '(if-some [a false]
                                 "false"
                                 "true"))))
    (is (= "nil" (eval-form '(if-some [a nil]
                               "true"
                               "nil"))))))

(deftest some-forms-test
  (testing "some->"
    (is (= 42 (eval-form '(some-> {:a {:b {:c 42}}}
                                  :a
                                  :b
                                  :c))))
    (is (= 42 (eval-form '(some-> {"a" {"b" {"c" 42}}}
                                  (get "a")
                                  (get "b")
                                  (get "c")))))
    (is (nil? (eval-form '(some-> {"a" {"b" {"c" 42}}}
                                  (get "a")
                                  (get "missing"))))))

  (testing "some->>"
    (is (= ["key:a" "key:b" "key:c"]
           (eval-form '(some->> [:a :b :c]
                                (map str)
                                (map #(str "key" %))))))
    (is (nil? (eval-form '(some->> [:a :b :c]
                                   (filter nil?)
                                   seq
                                   (throw)))))))

(deftest when-forms-test
  (testing "when"
    (is (= 42 (eval-form '(when true 42))))
    (is (nil? (eval-form '(when false 42)))))

  (testing "when-let"
    (is (= 42 (eval-form '(when-let [a 42]
                            (* a 2)
                            a)))))

  (testing "when-some"
    (is (= "false" (eval-form '(when-some [a false]
                                 "true"
                                 (str a)))))
    (is (nil? (eval-form '(when-some [a nil]
                            "nil"
                            "true"))))))
