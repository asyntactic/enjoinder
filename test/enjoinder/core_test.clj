(ns enjoinder.core-test
  (:use clojure.test
        enjoinder.core))

(deftest test-dissoc-ids
  (is (= {:foo 1 :bar 2 :baz 3}
         (dissoc-ids {:id 1 :user_id 2 :foo 1 :bar 2 :baz 3})))
  (is (= {"foo" 1 "bar" 2 "baz" 3}
         (dissoc-ids {"id" 1 "user_id" 2 "foo" 1 "bar" 2 "baz" 3}))))

(deftest test-dissoc-all-ids
  (is (= {:foo 1 :bar 2 :baz 3
          :quux {:zork 1 :paid true}}
         (dissoc-all-ids {:id 1 :user_id 2 :foo 1 :bar 2 :baz 3
                          :quux {:id 1 :zork 1 :paid true}}))))

(deftest test-contains-structure?
  (testing "non-associative subset"
    (is (contains-structure? [:foo :bar :baz :quux] [:baz]))
    (is (not (contains-structure? [:foo :bar :baz :quux] [:absent]))))

  (testing "associative subset"
    (is (contains-structure? {:foo :bar :baz :quux} {:baz :quux}))
    (is (not (contains-structure? {:foo :bar :baz :quux}
                                  {:baz :absent})))

    (is (contains-structure? {:foo :bar 
                              :baz {:flork :frob}
                              :quux nil}
                             {:foo :bar
                              :baz {:flork :frob}}))
    (is (not (contains-structure? {:foo :bar 
                                   :baz {:flork :frob}
                                   :quux nil}
                                  {:foo :bar
                                   :baz {:flork :absent}})))
    )

  (testing "combined associative and non-associative"
    (is (contains-structure?
         {:foo 1
          :bar {:a 2
                :b [3 4 {:x 5
                         :y 6}]}
          :baz 7
          :quux :ignored}
         {:foo 1
          :bar {:a 2
                :b [4 {:x 5}]}
          :baz 7}))

    (is (not (contains-structure?
              {:foo 1
               :bar {:a 2
                     :b [3 4 {:x 5
                              :y 6}]}
               :baz 7
               :quux :ignored}
              {:foo 1
               :bar {:a 2
                     :b [4 {:x :absent}]}
               :baz 7})))

    (is (contains-structure?
         [{:foo :bar
           :stuff [{:baz :quux}]}]
         [{:stuff [{:baz :quux}]}]))

    (is (not (contains-structure?
              [{:foo :bar
                :stuff [{:baz :quux}]}]
              [{:baz :quux}])))
))
