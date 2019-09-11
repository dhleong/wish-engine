(ns wish-engine.runtime-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [wish-engine.runtime :refer [clean-form]]))

(deftest clean-form-test
  (testing "Ignore unusual (but unrelated) forms"
    (is (map? (clean-form
                '{:attrs
                  {:5e/starting-eq
                   [(:mace :warhammer)
                    (:scale-mail :leather-armor :chain-mail)
                    ([:light-crossbow :crossbow-bolt] {:type :weapon
                                                       :category :simple})
                    (:priests-pack :explorers-pack)
                    [:shield {:kind :holy-symbol}]]}})))))

