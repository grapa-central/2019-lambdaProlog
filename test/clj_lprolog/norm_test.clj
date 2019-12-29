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
  (t/is (= '(λ 2 [A 3 3 (2 1)]) (nor/rewrite-suspension ['(λ 2 A) 1 1 '()]))))

;; Tests on explicit beta-reduction

(t/deftest beta-step-test
  (t/is (= '[#{0} 1 0 ([A 0])] (nor/beta-step '((λ 1 #{0}) A))))
  (t/is (= '([(λ 1 #{0}) 1 0 ([A 0])] B)
           (nor/beta-step '((λ 2 #{0}) A B))))
  (t/is (= '([(λ 1 A) 2 2 ([B 2] [#{0} 1])] C)
           (nor/beta-step '((λ 2 [A 2 3 (2 [#{0} 1])]) B C)))))

(t/deftest beta-reduce-test
  (t/is (= 'A (nor/explicit-reduce '((λ 1 #{0}) A))))
  (t/is (= '(A B) (nor/explicit-reduce '((λ 1 #{0}) A B))))
  (t/is (= 'B (nor/explicit-reduce '((λ 2 #{0}) A B))))
  (t/is (= 'A (nor/explicit-reduce '((λ 2 #{1}) A B))))
  (t/is (= '(A B) (nor/explicit-reduce '((λ 2 (#{1} #{0})) A B)))))

(defn test-reduce-meta
  [t] (binding [*print-meta* true]
        (do (pr (nor/explicit-reduce (second (typ/elaborate-term t))))
            (println ""))))

(defn test-normalize-meta
  [t] (binding [*print-meta* true]
        (do (pr (nor/normalize (second (typ/elaborate-term t))))
            (println ""))))

;;(test-normalize-meta '((λ 2 (#{0} A)) A B))
(test-normalize-meta '((λ 1 (λ 1 #{1})) (λ 3 (A #{0}))))
;;(test-reduce-meta '((λ 2 (λ 1 (#{0} A))) A B))
