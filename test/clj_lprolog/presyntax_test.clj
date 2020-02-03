(ns clj-lprolog.presyntax-test
  (:require [clj-lprolog.presyntax :as syn]
            [clojure.test :as t]
            [clj-lprolog.utils :as u]))

;; Tests on proper kernel terms

(t/deftest proper-kernel-lambda?-test
  (t/testing "positive"
    (t/is (syn/proper-lambda? '(λ 2 #{1}))))
  (t/testing "negative"
    (t/is (not (syn/proper-lambda? '(l 2 #{1}))))
    (t/is (not (syn/proper-lambda? '(λ -1 #{1}))))
    (t/is (not (syn/proper-lambda? '(λ 1 ()))))))

(t/deftest proper-kernel-application?-test
  (t/testing "positive"
    (t/is (syn/proper-application? '(A #{1} #{2}))))
  (t/testing "negative"
    (t/is (not (syn/proper-application? '())))
    (t/is (not (syn/proper-application? '(A ()))))))

(t/deftest proper-kernel-term?-test
  (t/is (syn/proper-kernel-term? #{1}))
  (t/is (syn/proper-kernel-term? 'A))
  (t/is (syn/proper-kernel-term? '(λ 2 (#{1} #{2}))))
  (t/is (syn/proper-kernel-term? '(A B)))
  (t/is (syn/proper-kernel-term? '(print "hello world"))))

;; Tests on proper type syntax

(t/deftest proper-arrow-type?-test
  (t/testing "positive"
    (t/is (syn/proper-arrow-type? '(-> A o)))
    (t/is (syn/proper-arrow-type? '(-> (-> A B) A B))))
  (t/testing "negative"
    (t/is (not (syn/proper-arrow-type? 'A)))
    (t/is (not (syn/proper-arrow-type? '(-> A ()))))))

(t/deftest proper-applied-type-constructor?-test
  (t/testing "positive"
    (t/is (syn/proper-applied-type-constructor? '(pair nat bool))))
  (t/testing "negative"
    (t/is (not (syn/proper-applied-type-constructor? '(list ()))))))

(t/deftest proper-type?-test
  (t/is (syn/proper-type? 'o))
  (t/is (syn/proper-type? 'i))
  (t/is (syn/proper-type? 'A))
  (t/is (syn/proper-type? '(-> (-> A o) A o))))

;; Tests on user terms syntax

(t/deftest bound-or-const?-test
  (t/testing "positive"
    (t/is (syn/bound-or-const? 'a)))
  (t/testing "negative"
    (t/is (not (syn/bound-or-const? 'A)))
    (t/is (not (syn/bound-or-const? ())))
    (t/is (not (syn/bound-or-const? -1)))))

(t/deftest lambda?-test
  (t/testing "positive"
    (t/is (syn/lambda? '(λ [x y] x))))
  (t/testing "negative"
    (t/is (not (syn/lambda? '(l [x y] 1))))
    (t/is (not (syn/lambda? '(λ 1 x))))))

(t/deftest parse-test
  (t/testing "successful parsing"
    (t/is (= [:ok '(S N)] (syn/parse '(S N))))
    (t/is (= [:ok '(λ 2 (#{1} #{0}))] (syn/parse '(λ [x y] (x y)))))
    (t/is (= [:ok '(λ 1 (+ #{0} zero))] (syn/parse '(λ [x] (+ x zero)))))
    (t/is (= [:ok '(λ 1 (A #{0}))] (syn/parse '(λ [x] (A x)))))
    (t/is (= [:ok '(λ 1 (λ 1 #{0}))] (syn/parse '(λ [f] (λ [x] x)))))
    (t/is (= [:ok '((λ 2 (#{1} #{0})) (λ 1 #{0}))]
             (syn/parse '((λ [x y] (x y)) (λ [x] x))))))
  (t/testing "failed parsing"
    (t/is (u/ko-expr? (syn/parse '())))))

;; Test on prolog vernacular syntax

(t/deftest user-type-dec?-test
  (t/testing "positive"
    (t/is (syn/user-type-dec? 'nat))
    (t/is (syn/user-type-dec? '(list A)))
    (t/is (syn/user-type-dec? '(pair A B))))
  (t/testing "negative"
    (t/is (not (syn/user-type-dec? 'A)))
    (t/is (not (syn/user-type-dec? '(list (list A)))))
    (t/is (not (syn/user-type-dec? '(pair a b))))))

(t/deftest parse-goal-test
  (t/is (u/ok-expr? (syn/parse-goal '(even O))))
  (t/is (u/ok-expr? (syn/parse-goal '(Π (n :> i) (=> (P n) (P (S (S n))))))))
  (t/is (u/ok-expr? (syn/parse-goal '(print (λ [x] x))))))

(t/deftest parse-clause-test
  (t/testing "positive"
    (t/is (u/ok-expr? (syn/parse-clause '((even O)))))
    (t/is (u/ok-expr? (syn/parse-clause '((even (S N)) :- (even N)))))
    (t/is (u/ok-expr?
           (syn/parse-clause '((parity P) :-
                               (Π (n :> i) (=> (P n) (P (S (S n))))))))))
  (t/testing "negative"
    (t/is (u/ko-expr? (syn/parse-clause '((even (S N)) :-))))))
