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

(t/deftest user-const?-test
  (t/testing "positive"
    (t/is (syn/user-const? 'zero))
    (t/is (syn/user-const? 'succ)))
  (t/testing "negative"
    (t/is (not (syn/user-const? 'A)))))

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

(t/deftest user-type?-test
  (t/testing "positive"
    (t/is (syn/user-type? 'bool)))
  (t/testing "negative"
    (t/is (not (syn/user-type? 'Bool)))))

(t/deftest arrow-type?-test
  (t/testing "positive"
    (t/is (syn/arrow-type? '(-> t1 t2))))
  (t/testing "negative"
    (t/is (not (syn/arrow-type? '(t1 t2))))))

(t/deftest applied-type-constructor?-test
  (t/testing "positive"
    (t/is (syn/applied-type-constructor? '(list A)))
    (t/is (syn/applied-type-constructor? '(pair bool nat)))
    (t/is (syn/applied-type-constructor? '(-> nat o)))) ;; Yes
  (t/testing "negative"
    (t/is (not (syn/applied-type-constructor? 'A)))
    (t/is (not (syn/applied-type-constructor? '(list))))))

(t/deftest destruct-arrow-test
  (t/is (= ['(A) 'B] (syn/destruct-arrow '(-> A B) 1)))
  (t/is (= ['(A) '(-> B C)] (syn/destruct-arrow '(-> A B C) 1)))
  (t/is (= ['(A B C) '(-> D E)] (syn/destruct-arrow '(-> A B C D E) 3))))

(t/deftest flatten-arrow-test
  (t/is (= 'A (syn/flatten-arrow 'A)))
  (t/is (= '(-> A B C) (syn/flatten-arrow '(-> A (-> B C)))))
  (t/is (= '(-> A B C D) (syn/flatten-arrow '(-> A (-> B (-> C D)))))))
