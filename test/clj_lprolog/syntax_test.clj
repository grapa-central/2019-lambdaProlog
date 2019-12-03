(ns clj-lprolog.syntax-test
  (:require [clj-lprolog.syntax :as syn]
            [clojure.test :as t]))

(t/deftest bound?-test
  (t/testing "positive"
    (t/is (syn/bound? 42)))
  (t/testing "negative"
    (t/is (not (syn/bound? 'A)))
    (t/is (not (syn/bound? ())))
    (t/is (not (syn/bound? -1)))))

(t/deftest free?-test
  (t/testing "positive"
    (t/is (syn/free? 'A)))
  (t/testing "negative"
    (t/is (not (syn/free? 12)))
    (t/is (not (syn/free? ())))))

(t/deftest lambda?-test
  (t/testing "positive"
    (t/is (syn/lambda? '(λ 2 1))))
  (t/testing "negative"
    (t/is (not (syn/lambda? '(l 2 1))))
    (t/is (not (syn/lambda? '(λ -1 1))))
    (t/is (not (syn/lambda? '(λ 1 ()))))))

(t/deftest application?-test
  (t/testing "positive"
    (t/is (syn/application? '(A 1 2))))
  (t/testing "negative"
    (t/is (not (syn/application? '())))
    (t/is (not (syn/application? '(A))))
    (t/is (not (syn/application? '(A ()))))))

(t/deftest kernel-term?-test
  (t/is (syn/kernel-term? 1))
  (t/is (syn/kernel-term? 'A))
  (t/is (syn/kernel-term? '(λ 2 (1 0))))
  (t/is (syn/kernel-term? '(A B))))
