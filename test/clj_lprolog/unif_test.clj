(ns clj-lprolog.unif-test
  (:require [clj-lprolog.utils :as u]
            [clj-lprolog.unif :as uni]
            [clojure.test :as t]
            [clj-lprolog.typecheck :as typ]
            [clj-lprolog.norm :as nor]))

(t/deftest subst-test
  (t/is '+ (uni/subst 'A '+ 'A))
  (t/is 'B (uni/subst 'A '+ 'B))
  (t/is '(λ 1 (S O)) (uni/subst 'A 'O '(λ 1 (S A))))
  (t/is '((λ 3 #{0}) B C (λ 3 #{0})) (uni/subst 'A '(λ 3 #{0}) '(A B C A))))

(t/deftest unif-var?-test
  (t/testing "positive"
    (t/is (uni/unif-var? '(λ 0 (A)))))
  (t/testing "negative"
    (t/is (not (uni/unif-var? '(λ 1 (A)))))
    (t/is (not (uni/unif-var? '(λ 0 (A B)))))
    (t/is (not (uni/unif-var? '(λ 0 (O)))))))

(t/deftest term-unknowns-test
  (t/is (= #{} (uni/term-unknowns '(λ 1 #{0}))))
  (t/is (= #{'A} (uni/term-unknowns '(λ 1 A))))
  (t/is (= #{'A 'B} (uni/term-unknowns '(A B A)))))

(t/deftest trivial-test
  (t/is (= '() (uni/trivial '([(λ 0 (#{0})) (λ 0 (A))]))))
  (t/is (= '([(λ 0 (A)) (λ 0 (A B))]) (uni/trivial '([(λ 0 (A)) (λ 0 (A B))]))))
  (t/is (= '([(λ 1 (#{0})) (λ 2 (O))])
           (uni/trivial '([(λ 1 (#{0})) (λ 1 (F))] [(λ 0 (F)) (λ 1 (O))])))))

(t/deftest simpl-test
  (t/testing "positive"
    (t/is (= '[:ok ()] (uni/simpl '([(λ 0 (#{0})) (λ 0 (A))]))))
    (t/is (= '[:ok ([(λ 2 (A)) (λ 2 (#{1}))])]
             (uni/simpl '([(λ 2 (S A)) (λ 2 (S #{1}))])))))
  (t/testing "negative"
    (t/is (u/ko-expr? (uni/simpl '([(λ 2 (#{0})) (λ 2 (#{1}))]))))))

(defn type-and-match
  [t1 t2] (u/ok> (typ/elaborate-term {} t2) :as [_ t2]
                 (typ/check-and-elaborate-term {} t1 (typ/type-of t2)) :as [_ t1]
                 (uni/match t1 t2)))

(t/deftest match-test
  (t/is (= '#{[A (λ 2 (+ (H0 #{1} #{0}) (H1 #{1} #{0})))] ;; Imitation
              [A (λ 2 (#{0}))] [A (λ 2 (#{1}))]} ;; Projections
           (type-and-match '(λ 2 (A #{0} #{1})) '(λ 2 (+ #{1} #{0})))))
  (t/is (= '#{[A (λ 2 (#{0} (H0 #{1} #{0})))]} ;; Projection
           (type-and-match '(λ 2 (A #{0} #{1})) '(λ 2 ((λ 1 (#{0} O)) #{1}))))))

(defn test-match-meta
  [t1 t2] (binding [*print-meta* true]
            (do (println (type-and-match t1 t2))
                (pr (type-and-match t1 t2))
                (println "") (println ""))))

;;(test-match-meta '(λ 2 (A #{0} #{1})) '(λ 2 (+ #{1} #{0})))
;;(test-match-meta '(λ 2 (A #{0} #{1})) '(λ 2 ((λ 1 (#{0} O)) #{1})))
