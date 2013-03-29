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

