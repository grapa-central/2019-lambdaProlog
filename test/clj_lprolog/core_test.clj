(ns clj-lprolog.core-test
  (:require [clj-lprolog.core :as sut]
            [clojure.test :as t]))

(t/deftest dummy-test
  (t/testing "Dummy test."
    (t/is (= 1 1))
    (t/is (not= 0 1))))
