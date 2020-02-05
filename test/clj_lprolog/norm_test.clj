(ns clj-lprolog.norm-test
  (:require [clj-lprolog.norm :as nor]
            [clojure.test :as t]
            [clj-lprolog.typecheck :as typ]))

;; Tests on syntax simplification

(t/deftest flatten-lambda-test
  (t/is (= '(λ 3 #{0}) (nor/flatten-lambda '(λ 3 #{0}))))
  (t/is (= '(λ 3 #{0}) (nor/flatten-lambda '(λ 1 (λ 1 (λ 1 #{0})))))))

(t/deftest flatten-zero-lambda-test
  (t/is (= '(A B) (nor/flatten-zero-lambda '(λ 0 (A B)))))
  (t/is (= '(λ 1 (A B)) (nor/flatten-zero-lambda '(λ 1 (A B))))))

(t/deftest lambda-form-test
  (t/is (= '(λ 0 A) (nor/lambda-form 'A)))
  (t/is (= '(λ 1 A) (nor/lambda-form '(λ 1 A)))))

(t/deftest flatten-application-test
  (t/is (= '(A B C) (nor/flatten-application '((A B) C))))
  (t/is (= '(A B C D) (nor/flatten-application '(((A B) C) D))))
  (t/is (= '(A (B C) D) (nor/flatten-application '((A (B C)) D)))))

(t/deftest rewrite-suspension-test
  (t/is (= '([t1 1 2 ()] [t2 1 2 ()])
           (nor/rewrite-suspension ['(t1 t2) 1 2 ()])))
  (t/is (= 'succ (nor/rewrite-suspension ['succ 1 2 ()])))
  (t/is (= '(λ 2 [A 3 3 (2 1)]) (nor/rewrite-suspension ['(λ 2 A) 1 1 '()]))))

;; Tests on implicit beta-reduction

(t/deftest implicit-subst-test
  (t/is (= '(S O) (nor/implicit-subst 2 'O '(S #{2}))))
  (t/is (= '((λ 1 #{0}) A) (nor/implicit-subst 'A '((λ 1 #{0}) #{0}))))
  (t/is (= '((λ 1 A) A) (nor/implicit-subst 'A '((λ 1 #{1}) #{0}))))
  (t/is (= '((λ 1 #{1}) A) (nor/implicit-subst 1 'A '((λ 1 #{1}) #{1})))))

(t/deftest implicit-reduce-test
  (t/is (= '(S O) (nor/implicit-reduce '((λ 1 (S #{0})) O))))
  (t/is (= '(A B) (nor/implicit-reduce '((λ 2 (#{0} #{1})) B A))))
  (t/is (= '(λ 1 (#{0} B)) (nor/implicit-reduce '((λ 2 (#{0} #{1})) B))))
  (t/is (= 'A (nor/implicit-reduce '((λ 1 (#{0} A)) (λ 1 #{0}))))))

;; Tests on explicit beta-reduction

(t/deftest beta-step-test
  (t/is (= '[#{0} 1 0 ([A 0])] (nor/beta-step '((λ 1 #{0}) A))))
  (t/is (= '([(λ 1 #{0}) 1 0 ([A 0])] B)
           (nor/beta-step '((λ 2 #{0}) A B))))
  (t/is (= '([(λ 1 A) 2 2 ([B 2] [#{0} 1])] C)
           (nor/beta-step '((λ 2 [A 2 3 (2 [#{0} 1])]) B C)))))

(t/deftest explicit-beta-reduce-test
  (t/is (= 'A (nor/explicit-reduce '((λ 1 #{0}) A))))
  (t/is (= '(A B) (nor/explicit-reduce '((λ 1 #{0}) A B))))
  (t/is (= 'B (nor/explicit-reduce '((λ 2 #{0}) A B))))
  (t/is (= 'A (nor/explicit-reduce '((λ 2 #{1}) A B))))
  (t/is (= '(A B) (nor/explicit-reduce '((λ 2 (#{1} #{0})) A B)))))

;; Tests on normalization

(t/deftest evaluable?-test
  (t/testing "positive"
    (t/is (nor/evaluable? '(+ 3 4)))
    (t/is (nor/evaluable? '(* (+ 1 2) (+ 3 4)))))
  (t/testing "negative"
    (t/is (not (nor/evaluable? '(+ 3 A))))
    (t/is (not (nor/evaluable? (second (typ/elaborate-term {} '(+ 3))))))))

(t/deftest simplify-term-test
  (t/is (= 'S (nor/simplify-term '(λ 0 (λ 0 S)))))
  (t/is (= 12 (nor/simplify-term '((λ 1 (+ 5 #{0})) 7))))
  (t/is (= 'S (nor/simplify-term '(((((((((((((((((((S)))))))))))))))))))))))

(t/deftest lift-indices-test
  (t/is (= #{2} (nor/lift-indices 2 #{0})))
  (t/is (= '(λ 1 #{0}) (nor/lift-indices 1 '(λ 1 #{0}))))
  (t/is (= '(λ 1 #{3}) (nor/lift-indices 2 '(λ 1 #{1}))))
  (t/is (= '((λ 1 #{2}) #{1}) (nor/lift-indices 1 '((λ 1 #{1}) #{0})))))

(defn eta
  [t] (nor/norm-eta (second (typ/elaborate-term '{succ (-> int int)} t))))

(t/deftest norm-eta-test
  (t/is (= '(λ 2 #{0}) (eta '(λ 2 #{0}))))
  (t/is (= '(λ 1 (succ #{0}))
           (eta '(λ 0 succ))))
  (t/is (= '(λ 3 ((λ 2 (S #{0})) #{1} #{0})) (eta '(λ 1 (λ 2 (S #{0})))))))

(defn normalize
  [t] (nor/normalize (second (typ/elaborate-term {} t))))

(t/deftest normalize-test
  (t/is '(λ 0 (A)) (normalize '((λ 1 #{0}) A)))
  (t/is (= '(λ 2 (A #{0})) (normalize '((λ 1 (λ 1 #{1})) (λ 1 (A #{0})))))))

(defn test-normalize-meta
  [t] (binding [*print-meta* true]
        (do (pr (normalize t))
            (println ""))))

;;(test-normalize-meta '((λ 2 (#{0} A)) A B))
;;(test-normalize-meta '((λ 1 (λ 1 #{1})) (λ 3 (A #{0}))))
;;(test-reduce-meta '((λ 2 (λ 1 (#{0} A))) A B))
