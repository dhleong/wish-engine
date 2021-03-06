(ns wish-engine.runtime-eval-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [wish-engine.core :as core]
            [wish-engine.test-util :refer [eval-form eval-state]]))

(deftest basic-test
  (testing "Math"
    (is (= 42 (eval-form '(+ 20 22)))))

  (testing "let"
    (is (= 42 (eval-form '(let [n 21]
                            (* 2 n))))))
  (testing "if"
    (is (= true (eval-form '(if true
                              true))))
    (is (nil? (eval-form '(if false
                            true)))))

  (testing "Exported functions"
    (is (= "1st" (eval-form '(ordinal 1))))
    (is (= true (eval-form '(has? #{:mreynolds} [:mreynolds])))))

  (testing "Macro/fn evaluation"
    (is (= 9001 (eval-form '(when 42 9001))))

    (let [f (eval-form '(fn [v]
                          (cond
                            (<= 42 v 9001) :firefly
                            (> v 9001) :serenity
                            :else :alliance)))]
      (is (= :firefly (f 42)))
      (is (= :serenity (f 9002)))
      (is (= :alliance (f 0))))))

(deftest doseq-test
  (testing "Support doseq"
    (let [state (eval-state
                  '(doseq [list-id [:captain :crew]]
                     (declare-list
                       list-id
                       {:id :sidearm})))]
      (is (= [{:id :sidearm}]
             (core/inflate-entities
               state
               (get-in state [:lists :captain]))))
      (is (= (core/inflate-entities
               state
               (get-in state [:lists :captain]))
             (core/inflate-entities
               state
               (get-in state [:lists :crew])))))))

(deftest fn-test
  (testing "Support destructuring args"
    (let [f (eval-form '(fn [{:keys [id]}]
                          (name id)))]
      (= "serenity"
         (f {:id :serenity}))))

  (testing "Support special map destructuring convenience"
    (let [f (eval-form '(fn [#{id}]
                          (name id)))]
      (= "serenity"
         (f {:id :serenity}))))

  (testing "Support vector destructuring"
    (let [f (eval-form '(fn [[_ id]]
                          (name id)))]
      (= "serenity"
         (f [:id :serenity])))))

(deftest quoted-forms-test
  (testing "Handle quoted forms"
    (let [form (eval-form
                 '{:attrs
                   {:5e/starting-eq
                    '[(:mace :warhammer)
                      (:scale-mail :leather-armor :chain-mail)
                      ([:light-crossbow :crossbow-bolt] {:type :weapon
                                                         :category :simple})
                      (:priests-pack :explorers-pack)
                      [:shield {:kind :holy-symbol}]]}})
          eq (-> form :attrs :5e/starting-eq)]
      (is (map? form))
      (is (vector? eq))

      (is (list? (first eq)))
      (is (vector? (last eq))))))

(deftest error-handling-test
  (testing "Better errors from unknown fn calls"
    (let [err (try (eval-form '(declare-pants
                                 {:type :captain/tight}))
                   nil
                   (catch :default e e))
          data (ex-data err)]
      (is (some? err))
      (is (some? data))
      (is (= {:unknown-fn "declare-pants"
              :original-form "(declare-pants {:type :captain/tight})"}
             data)))))

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

(deftest for-form-test
  (testing "for: simple"
    (is (= [2 4 6 8 10]
           (eval-form '(for [v (range 5)]
                         (* 2 (inc v)))))))

  (testing "for: multiple seqs"
    (is (= [[0 2] [0 4] [0 6]
            [1 2] [1 4] [1 6]]
           (eval-form '(for [i (range 2)
                             v (range 3)]
                         [i (* 2 (inc v))]))))))

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

(deftest bool-forms
  (testing "and"
    (is (= 9001 (eval-form '(and true
                               42
                               9001))))
    (is (false? (eval-form '(and true false))))
    (is (nil? (eval-form '(and nil))))
    (is (true? (eval-form '(and)))))

  (testing "or"
    (is (= 42 (eval-form '(or (when false 22)
                              (if-not true 32)
                              42))))
    (is (false? (eval-form '(or false))))
    (is (nil? (eval-form '(or))))))

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
                                   (vec)))))))

(deftest when-forms-test
  (testing "when"
    (is (= 42 (eval-form '(when true 42))))
    (is (nil? (eval-form '(when false 42)))))

  (testing "when-let"
    (is (= 42 (eval-form '(when-let [a 42]
                            (* a 2)
                            a)))))

  (testing "when-first"
    (is (= 42 (eval-form '(when-first [a [42]]
                            (* a 2)
                            a))))
    (is (nil? (eval-form '(when-first [a []]
                            (* a 2))))))

  (testing "when-some"
    (is (= "false" (eval-form '(when-some [a false]
                                 "true"
                                 (str a)))))
    (is (nil? (eval-form '(when-some [a nil]
                            "nil"
                            "true")))))

  (testing "when-not"
    (is (= "true" (eval-form '(when-not false
                                42
                                "true"))))
    (is (nil? (eval-form '(when-not true
                            "nil"
                            "true"))))))

(deftest wish-engine-macros-test
  (testing "on-state"
    (let [f (eval-form '(on-state
                          (assoc :flew? true)))]
      (is (ifn? f))
      (is (= {:flew? true}
             (f {}))))))
