(ns clj-lprolog.core-test
  (:require [clj-lprolog.core :as lp]
            [clj-lprolog.presyntax :as syn]
            [clojure.test :as t]))

;; Testing on macros

(t/deftest deftype-test
  (t/testing "nat"
    (do (lp/start)
        (lp/deftype 'nat)
        (t/is (= '#{nat} @lp/progtypes))))
  (t/testing "list"
    (do (lp/start)
        (lp/deftype '(list A))
        (t/is (= '#{(list A)} @lp/progtypes)))))

(t/deftest defconst-test
  (t/testing "bool"
    (do (lp/start)
        (lp/deftype 'bool)
        (lp/defconst 't 'bool) (lp/defconst 'f 'bool)
        (t/is (= '{t bool f bool} @lp/progconsts))))
  (t/testing "nat"
    (do (lp/start)
        (lp/deftype 'nat)
        (lp/defconst 'zero 'nat) (lp/defconst 'succ '(-> nat nat))
        (t/is (= '{zero nat succ (-> nat nat)} @lp/progconsts))))
  (t/testing "list"
    (do (lp/start)
        (lp/deftype '(list A))
        (lp/defconst 'ni '(list A))
        (lp/defconst 'cs '(-> B (list B) (list B))))))

(t/deftest defpred-test
  (t/testing "even"
    (do (lp/start)
        (lp/defpred 'even '(-> i o))
        (t/is (= @lp/progpreds {'even ['(-> i o) '()]}))))
  (t/testing "even and odd"
    (do (lp/start)
        (lp/defpred 'even '(-> i o))
        (lp/defpred 'odd '(-> i o))
        (t/is (= (get @lp/progpreds 'even) (get @lp/progpreds 'odd))))))

(t/deftest addclause-test
  (t/testing "even"
    (do (lp/start)
        (lp/defpred 'even '(-> i o))
        (lp/addclause '((even O)))
        (lp/addclause '((even (S (S N))) :- (even N)))
        (t/is (= @lp/progpreds {'even ['(-> i o)
                                        '([(even O) ()]
                                          [(even (S (S N))) ((even N))])]}))))
  (t/testing "is the identity"
    (do
      (lp/start)
      (lp/defpred 'isid '(-> (-> A A) o))
      (lp/addclause '((isid (λ [x] x))))
      (t/is (= @lp/progpreds {'isid ['(-> (-> A A) o)
                                      '([(isid (λ 1 #{0})) ()])]})))))
