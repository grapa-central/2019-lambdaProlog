(ns clj-lprolog.presyntax-test
  (:require [clj-lprolog.presyntax :as syn]
            [clojure.test :as t]))

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
    (t/is (not (syn/free? 'a)))
    (t/is (not (syn/free? ())))))

(t/deftest lambda?-test
  (t/testing "positive"
    (t/is (syn/lambda? '(λ [x y] x))))
  (t/testing "negative"
    (t/is (not (syn/lambda? '(l [x y] 1))))
    (t/is (not (syn/lambda? '(λ [x] 1))))
    (t/is (not (syn/lambda? '(λ [x] ()))))))

(t/deftest application?-test
  (t/testing "positive"
    (t/is (syn/application? '(A x y))))
  (t/testing "negative"
    (t/is (not (syn/application? '())))
    (t/is (not (syn/application? '(A))))
    (t/is (not (syn/application? '(A ()))))))

(t/deftest kernel-term?-test
  (t/is (syn/user-term? 'x))
  (t/is (syn/user-term? 'A))
  (t/is (syn/user-term? '(λ [x y] (x y))))
  (t/is (syn/user-term? '(A B))))

(t/deftest parse-test
  (t/testing "successful parsing"
    (t/is (= (syn/parse '(λ [x y] (x y))) '(λ 2 (0 1))))
    (t/is (= (syn/parse '(λ [x] (A x))) '(λ 1 (A 0))))
    (t/is (= (syn/parse '((λ [x y] (x y)) (λ [x] x))) '((λ 2 (0 1)) (λ 1 0)))))
  (t/testing "failed parsing"
    (t/is (nil? (syn/parse '())))
    (t/is (nil? (syn/parse '(λ [x y] z))))))
