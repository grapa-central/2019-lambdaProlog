(ns clj-lprolog.presyntax-test
  (:require [clj-lprolog.presyntax :as syn]
            [clojure.test :as t]
            [clj-lprolog.utils :as u]))

;; Tests on proper kernel terms

(t/deftest proper-kernel-lambda?-test
  (t/testing "positive"
    (t/is (syn/proper-lambda? '(λ 2 1))))
  (t/testing "negative"
    (t/is (not (syn/proper-lambda? '(l 2 1))))
    (t/is (not (syn/proper-lambda? '(λ -1 1))))
    (t/is (not (syn/proper-lambda? '(λ 1 ()))))))

(t/deftest proper-kernel-application?-test
  (t/testing "positive"
    (t/is (syn/proper-application? '(A 1 2))))
  (t/testing "negative"
    (t/is (not (syn/proper-application? '())))
    (t/is (not (syn/proper-application? '(A))))
    (t/is (not (syn/proper-application? '(A ()))))))

(t/deftest proper-kernel-term?-test
  (t/is (syn/proper-kernel-term? 1))
  (t/is (syn/proper-kernel-term? 'A))
  (t/is (syn/proper-kernel-term? '(λ 2 (1 0))))
  (t/is (syn/proper-kernel-term? '(A B))))

;; Tests on proper type syntax

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

;; Tests on user terms syntax

(t/deftest bound?-test
  (t/testing "positive"
    (t/is (syn/bound? 'a)))
  (t/testing "negative"
    (t/is (not (syn/bound? 'A)))
    (t/is (not (syn/bound? ())))
    (t/is (not (syn/bound? -1)))))

(t/deftest free?-test
  (t/testing "positive"
    (t/is (syn/free? 'A)))
  (t/testing "negative"
    (t/is (not (syn/free? 'O)))
    (t/is (not (syn/free? 'a)))
    (t/is (not (syn/free? ())))))

(t/deftest lambda?-test
  (t/testing "positive"
    (t/is (syn/lambda? '(λ [x y] x))))
  (t/testing "negative"
    (t/is (not (syn/lambda? '(l [x y] 1))))
    (t/is (not (syn/lambda? '(λ 1 x))))))

(t/deftest application?-test
  (t/testing "positive"
    (t/is (syn/application? '(A x y))))
  (t/testing "negative"
    (t/is (not (syn/application? '())))
    (t/is (not (syn/application? '(A))))))

(t/deftest parse-test
  (t/testing "successful parsing"
    (t/is (= (syn/parse '(S N)) [:ok '(S N)]))
    (t/is (= (syn/parse '(λ [x y] (x y))) [:ok '(λ 2 (0 1))]))
    (t/is (= (syn/parse '(λ [x y] (+ x y))) [:ok '(λ 2 (+ 0 1))]))
    (t/is (= (syn/parse '(λ [x] (A x))) [:ok '(λ 1 (A 0))]))
    (t/is (= (syn/parse '((λ [x y] (x y)) (λ [x] x)))
             [:ok '((λ 2 (0 1)) (λ 1 0))])))
  (t/testing "failed parsing"
    (t/is (u/ko-expr? (syn/parse '())))
    (t/is (u/ko-expr? (syn/parse '(λ [x y] z))))))

;; Test on prolog vernacular syntax

(t/deftest pred?-test
  (t/testing "positive"
    (t/is (syn/pred? 'even)))
  (t/testing "negative"
    (t/is (not (syn/pred? 'Even)))))

(t/deftest applied-pred?-test
  (t/testing "positive"
    (t/is (syn/applied-pred? '(even O)))
    (t/is (syn/applied-pred? '(p A))))
  (t/testing "negative"
    (t/is (not (syn/applied-pred? '(Even O))))))

(t/deftest clause-body?-test
  (t/testing "positive"
    (t/is (syn/clause-body? '((even N))))
    (t/is (syn/clause-body? '())))
  (t/testing "negative"
    (t/is (not (syn/clause-body? '(even N))));; Careful with parenthesis !
    (t/is (not (syn/clause-body? '((even N) :- (even N) :- (even N)))))))

(t/deftest clause?-test
  (t/testing "positive"
    (t/is (syn/clause? '((even O))))
    (t/is (syn/clause? '((even (S (S N))) (even N))))
    (t/is (syn/clause? '((even (S (S N))) :- (even N)))))
  (t/testing "negative"
    (t/is (not (syn/clause? '(even O)))))) ;; Parenthesis !

;; Testing on defpred and addclause

(t/deftest defpred-test
  (t/testing "even"
    (do
      (swap! syn/progpreds (fn [_] {}))
      (syn/defpred 'even '(-> i o))
      (t/is (= @syn/progpreds {'even ['(-> i o) {}]}))))
  (t/testing "even and odd"
    (do
      (swap! syn/progpreds (fn [_] {}))
      (syn/defpred 'even '(-> i o))
      (syn/defpred 'odd '(-> i o))
      (t/is (= (get @syn/progpreds 'even) (get @syn/progpreds 'odd))))))

(t/deftest addclause-test
  (t/testing "even"
    (do
      (swap! syn/progpreds (fn [_] {}))
      (syn/defpred 'even '(-> i o))
      (syn/addclause '((even O)))
      (syn/addclause '((even (S (S N))) :- (even N)))
      (t/is (= @syn/progpreds {'even ['(-> i o)
                                      {'(even O) '()
                                       '(even (S (S N))) '((even N))}]}))))
  (t/testing "is the identity"
    (do
      (swap! syn/progpreds (fn [_] {}))
      (syn/defpred 'isid '(-> (-> A A) o))
      (syn/addclause '((isid (λ [x] x))))
      (t/is (= @syn/progpreds {'isid ['(-> (-> A A) o)
                                      {'(isid (λ 1 0)) '()}]})))))
