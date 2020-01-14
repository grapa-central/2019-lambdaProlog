(ns clj-lprolog.solve-test
  (:require [clj-lprolog.solve :as sol]
            [clojure.test :as t]
            [clj-lprolog.utils :as u]))

;; Tests on instantiation

(t/deftest instantiate-free-var-test
  (t/is (= 'N_42 (sol/instantiate-free-var 42 'N))))

(t/deftest instantiate-term-test
  (t/is (= '(λ 2 N_12) (sol/instantiate-term 12 '(λ 2 N))))
  (t/is (= '(M_2 N_2) (sol/instantiate-term 2 '(M N)))))

(t/deftest instantiate-pred-test
  (t/is (= '(even N_3) (sol/instantiate-pred 3 '(even N))))
  (t/is (= '(append (cs X_1 L1_1) L2_1 (cs X_1 L3_1))
           (sol/instantiate-pred 1 '(append (cs X L1) L2 (cs X L3))))))

(t/deftest instantiate-clause-test
  (t/is (= '[(even (succ N_2)) ((odd N_2))]
           (sol/instantiate-clause 2 '[(even (succ N)) ((odd N))]))))

;; Tests on solving algorithm

(t/deftest unify-clause-test
  (t/is (= '[:ok #{[{N (succ zero)} ((odd (succ zero)))]}]
           (sol/unify-clause '(even (succ (succ zero)))
                             '[(even (succ N)) ((odd N))])))
  (t/is (u/ko-expr?
         (sol/unify-clause '(even zero) '[(even (succ N)) ((odd N))]))))

(t/deftest solve-test
  (t/testing "positive"
    (t/is (= '[:ok {N (succ zero)}]
             (sol/solve
              '{even [(-> nat o) {(even zero) (),
                                  (even (succ (succ N))) ((even N))}]}
              '(even (succ (succ (succ N)))))))
    (t/is (= '[:ok {N zero}]
             (sol/solve
              '{even [(-> nat o) {(even zero) (),
                                  (even (succ N)) ((odd N))}]
                odd [(-> nat o) {(odd (succ zero)) (),
                                  (odd (succ N)) ((even N))}]}
              '(even (succ (succ N))))))))
