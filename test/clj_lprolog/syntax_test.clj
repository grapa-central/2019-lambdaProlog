(ns clj-lprolog.syntax-test
  (:require [clj-lprolog.syntax :as syn]
            [clojure.test :as t]))

(let [ns *ns*]
  (t/use-fixtures
    :once
    (fn [test-fn]
      (binding [*ns* ns]
        (test-fn)))))

;; Tests on kernel terms syntax

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
    (t/is (not (syn/free? 'S)))
    (t/is (not (syn/free? 12)))
    (t/is (not (syn/free? ())))))

(t/deftest primitive?-test
  (t/testing "positive"
    (t/is (syn/primitive? 'S))
    (t/is (syn/primitive? '+)))
  (t/testing "negative"
    (t/is (not (syn/primitive? '∀)))
    (t/is (not (syn/primitive? 'λ)))))

(t/deftest kernel-lambda?-test
  (t/testing "positive"
    (t/is (syn/proper-lambda? '(λ 2 1))))
  (t/testing "negative"
    (t/is (not (syn/proper-lambda? '(l 2 1))))
    (t/is (not (syn/proper-lambda? '(λ -1 1))))
    (t/is (not (syn/proper-lambda? '(λ 1 ()))))))

(t/deftest kernel-application?-test
  (t/testing "positive"
    (t/is (syn/proper-application? '(A 1 2))))
  (t/testing "negative"
    (t/is (not (syn/proper-application? '())))
    (t/is (not (syn/proper-application? '(A))))
    (t/is (not (syn/proper-application? '(A ()))))))

(t/deftest kernel-term?-test
  (t/is (syn/kernel-term? 1))
  (t/is (syn/kernel-term? 'A))
  (t/is (syn/kernel-term? '(λ 2 (1 0))))
  (t/is (syn/kernel-term? '(A B))))

;; Tests on type syntax

(t/deftest type-var?-test
  (t/testing "positive"
    (t/is (syn/type-var? 'A)))
  (t/testing "negative"
    (t/is (not (syn/type-var? 'a)))
    (t/is (not (syn/type-var? '())))))

(t/deftest nat-type?-test
  (t/testing "positive"
    (t/is (syn/nat-type? 'i)))
  (t/testing "negative"
    (t/is (not (syn/nat-type? 'I)))
    (t/is (not (syn/nat-type? '())))))

(t/deftest prop-type?-test
  (t/testing "positive"
    (t/is (syn/prop-type? 'o)))
  (t/testing "negative"
    (t/is (not (syn/prop-type? 'O)))
    (t/is (not (syn/prop-type? '())))))

(t/deftest proper-arrow-type?-test
  (t/testing "positive"
    (t/is (syn/proper-arrow-type? '(-> A o)))
    (t/is (syn/proper-arrow-type? '(-> (-> A B) A B))))
  (t/testing "negative"
    (t/is (not (syn/proper-arrow-type? 'A)))
    (t/is (not (syn/proper-arrow-type? '(-> A))))
    (t/is (not (syn/proper-arrow-type? '(-> A ()))))))

(t/deftest proper-type?-test
  (t/is (syn/proper-type? 'o))
  (t/is (syn/proper-type? 'i))
  (t/is (syn/proper-type? 'A))
  (t/is (syn/proper-type? '(-> (-> A o) A o))))

(t/deftest destruct-arrow-test
  (t/is (syn/destruct-arrow '(-> A B) 1) ['A 'B])
  (t/is (syn/destruct-arrow '(-> A B C) 1) ['A '(-> B C)])
  (t/is (syn/destruct-arrow '(-> A B C D E) 3) ['(A B C) '(-> D E)]))

(t/deftest flatten-arrow-test
  (t/is (syn/flatten-arrow 'A) 'A)
  (t/is (syn/flatten-arrow '(-> A (-> B C))) '(-> A B C))
  (t/is (syn/flatten-arrow '(-> A (-> B (-> C D)))) '(-> A B C D)))
