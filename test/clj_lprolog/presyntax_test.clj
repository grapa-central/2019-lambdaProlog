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

(t/deftest parse-test
  (t/testing "successful parsing"
    (t/is (= [:ok '(S N)] (syn/parse '(S N))))
    (t/is (= [:ok '(λ 2 (#{0} #{1}))] (syn/parse '(λ [x y] (x y)))))
    (t/is (= [:ok '(λ 1 (+ #{0} zero))] (syn/parse '(λ [x] (+ x zero)))))
    (t/is (= [:ok '(λ 1 (A #{0}))] (syn/parse '(λ [x] (A x)))))
    (t/is (= [:ok '((λ 2 (#{0} #{1})) (λ 1 #{0}))]
             (syn/parse '((λ [x y] (x y)) (λ [x] x))))))
  (t/testing "failed parsing"
    (t/is (u/ko-expr? (syn/parse '())))))

;; Test on prolog vernacular syntax

(t/deftest pred?-test
  (t/testing "positive"
    (t/is (syn/pred? 'even)))
  (t/testing "negative"
    (t/is (not (syn/pred? '())))))

(t/deftest applied-pred?-test
  (t/testing "positive"
    (t/is (syn/applied-pred? '(even O)))
    (t/is (syn/applied-pred? '(p A))))
  (t/testing "negative"
    (t/is (not (syn/applied-pred? '(() O))))))

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

(t/deftest user-type-dec?-test
  (t/testing "positive"
    (t/is (syn/user-type-dec? 'nat))
    (t/is (syn/user-type-dec? '(list A)))
    (t/is (syn/user-type-dec? '(pair A B))))
  (t/testing "negative"
    (t/is (not (syn/user-type-dec? 'A)))
    (t/is (not (syn/user-type-dec? '(list (list A)))))
    (t/is (not (syn/user-type-dec? '(pair a b))))))

;; Testing on macros

(t/deftest deftype-test
  (t/testing "nat"
    (do (syn/start)
        (syn/deftype 'nat)
        (t/is (= '#{nat} @syn/progtypes))))
  (t/testing "list"
    (do (syn/start)
        (syn/deftype '(list A))
        (t/is (= '#{(list A)} @syn/progtypes)))))

(t/deftest defconst-test
  (t/testing "bool"
    (do (syn/start)
        (syn/deftype 'bool)
        (syn/defconst 't 'bool) (syn/defconst 'f 'bool)
        (t/is (= '{t bool f bool} @syn/progconsts))))
  (t/testing "nat"
    (do (syn/start)
        (syn/deftype 'nat)
        (syn/defconst 'zero 'nat) (syn/defconst 'succ '(-> nat nat))
        (t/is (= '{zero nat succ (-> nat nat)} @syn/progconsts))))
  (t/testing "list"
    (do (syn/start)
        (syn/deftype '(list A))
        (syn/defconst 'ni '(list A))
        (syn/defconst 'cs '(-> B (list B) (list B))))))

(t/deftest defpred-test
  (t/testing "even"
    (do (syn/start)
        (syn/defpred 'even '(-> i o))
        (t/is (= @syn/progpreds {'even ['(-> i o) {}]}))))
  (t/testing "even and odd"
    (do (syn/start)
        (syn/defpred 'even '(-> i o))
        (syn/defpred 'odd '(-> i o))
        (t/is (= (get @syn/progpreds 'even) (get @syn/progpreds 'odd))))))

(t/deftest addclause-test
  (t/testing "even"
    (do (syn/start)
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
                                      {'(isid (λ 1 #{0})) '()}]})))))
