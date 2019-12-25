(ns clj-lprolog.typecheck-test
  (:require [clj-lprolog.utils :as u]
            [clj-lprolog.typecheck :as typ]
            [clj-lprolog.presyntax :as syn]
            [clojure.test :as t]))

(t/deftest substitute-ty-test
  (t/is (= 's (typ/substitute-ty 'Ty1 's 'Ty1)))
  (t/is (= '(-> Ty2 s o) (typ/substitute-ty 'Ty1 's '(-> Ty2 Ty1 o))))
  (t/is (= '(list nat) (typ/substitute-ty 'Ty1 'nat '(list Ty1)))))

(t/deftest apply-subst-ty-test
  (t/is (= '(-> x y) (typ/apply-subst-ty {'Ty1 'x 'Ty2 'y} '(-> Ty1 Ty2))))
  (t/is (= '(-> x Ty2) (typ/apply-subst-ty {'Ty1 'Ty3 'Ty3 'x} '(-> Ty1 Ty2)))))

(t/deftest mgu-ty-test
  (t/testing "positive"
    (t/is (= [:ok {'Ty1 'a}] (typ/mgu-ty 'Ty1 'a)))
    (t/is (= [:ok {'Ty1 'a 'Ty2 'a}]
             (typ/mgu-ty '(-> Ty1 (-> Ty1 Ty2)) '(-> Ty1 (-> a a)))))
    (t/is (= [:ok {'Ty1 'a 'Ty2 'a}]
             (typ/mgu-ty '(-> Ty1 (-> Ty1 Ty2)) '(-> a (-> Ty1 a)))))
    (t/is (= [:ok {'Ty1 'a 'Ty2 '(-> b c)}]
             (typ/mgu-ty '(-> a b c) '(-> Ty1 Ty2) )))
    (t/is (= [:ok {'Ty1 'nat}]
             (typ/mgu-ty '(list nat) '(list Ty1)))))
  (t/testing "negative"
    (t/is (u/ko-expr? (typ/mgu-ty 'a 'b)))
    (t/is (u/ko-expr? (typ/mgu-ty '(-> Ty1 Ty1) '(-> a b))))
    (t/is (u/ko-expr? (typ/mgu-ty '(-> Ty1 Ty2 Ty2) '(-> a Ty1 b))))
    (t/is (u/ko-expr? (typ/mgu-ty 'Ty1 '(-> Ty1 Ty2))))
    (t/is (u/ko-expr? (typ/mgu-ty '(pair Ty1 nat) '(pair bool Ty1))))))

(t/deftest rename-type-vars-test
  (t/is (= 'TyA42 (typ/rename-type-vars 42 'A)))
  (t/is (= 'i (typ/rename-type-vars 18 'i)))
  (t/is (= '(pair TyA0 TyB0) (typ/rename-type-vars 0 '(pair A B)))))

(t/deftest infer-term-test
  (t/testing "positive"
    (t/is (u/ok-expr? (typ/subst-infer-term {} #{0} ['i] 0)))
    (t/is (= [:ok 'i] (typ/infer-term '(S (S (S O))))))
    (t/is (= [:ok 'nat] (typ/infer-term {'zero 'nat} 'zero)))
    (t/is (= [:ok 'nat] (typ/infer-term {'succ '(-> nat nat)} '(succ N))))
    (t/is (= [:ok '(-> i i)] (typ/infer-term '(+ (* (S O) (S O))))))
    (t/is (u/ok-expr? (typ/infer-term '(λ 1 #{0}))))
    (t/is (u/ok-expr? (typ/infer-term '(λ 2 #{1}))))
    (t/is (u/ok-expr? (typ/infer-term '((λ 2 #{1}) O))))
    (t/is (= [:ok '(-> i i)] (typ/infer-term '((λ 2 (+ #{0} #{1})) O))))
    (t/is (= [:ok 'i] (typ/infer-term '(λ 0 (S O))))))
  (t/testing "negative"
    (t/is (u/ko-expr? (typ/infer-term '(S S))))
    (t/is (u/ko-expr? (typ/infer-term '((λ 1 (+ #{0} #{0})) S))))))

(t/deftest elaborate-term-test
  (t/testing "elaborated terms stay the same"
    (t/is (= '((λ 2 #{0}) A B)
             (second (typ/elaborate-term '((λ 2 #{0}) A B)))))))

(t/deftest check-and-elaborate-term-test
  (t/testing "positive"
    (t/is (u/ok-expr? (typ/check-and-elaborate-term {} '(S (S (S O))) 'i)))
    (t/is (u/ok-expr? (typ/check-and-elaborate-term {} '(λ 1 #{0}) '(-> A A))))
    (t/is (u/ok-expr? (typ/check-and-elaborate-term {} '(λ 2 #{1}) '(-> A i A))))
    (t/is (u/ok-expr? (typ/check-and-elaborate-term {} '((λ 2 #{1}) O) '(-> A i)))))
  (t/testing "negative"
    (t/is (u/ko-expr? (typ/check-and-elaborate-term {} '(λ 1 #{0}) '(-> A B))))))

;; Not easy to test metadata simply...

(defn test-elab-meta
  [t] (binding [*print-meta* true]
       (do (pr (typ/elaborate-term {} t)) (println ""))))

;; (test-elab-meta '(λ 0 O))
;; (test-check-elab-meta '(A O B) 'i)

(defn test-check-elab-meta
  [t ty] (binding [*print-meta* true]
        (do (pr (typ/check-and-elaborate-term {} t ty)))))

(defn elab-and-freevars
  [t ty] (u/ok> (typ/check-and-elaborate-term {} t ty) :as [_ t]
                (typ/get-freevar-types t)))

(t/deftest get-freevar-types-test
  (t/testing "positive"
    (t/is (= [:ok {'A 'o}] (elab-and-freevars '(λ 1 A) '(-> i o))))
    (t/is (= [:ok {'A 'i}] (elab-and-freevars '((λ 1 A) A) 'i)))
    (t/is (= [:ok {'A 'i 'B 'i}]
             (elab-and-freevars '((λ 2 (+ #{0} #{1})) A B) 'i))))
  (t/testing "negative"
    (t/is (u/ko-expr? (elab-and-freevars '((λ 1 (+ (A O) #{0})) A) 'i)))))

(t/deftest check-pred-test
  (t/testing "even"
    (do
      (syn/start)
      (syn/defpred 'even '(-> i o))
      (t/is (= {'A 'i}
               (nth
                (typ/elaborate-and-freevar-pred
                 {} (deref syn/progpreds) '(even A)) 2)))))
  (t/testing "predicate"
    (do
      (syn/start)
      (syn/defpred 'pred '(-> (-> i o) i o))
      (t/is (= {'F 'o}
               (nth
                (typ/elaborate-and-freevar-pred
                 {} (deref syn/progpreds) '(pred (λ 1 F) (S O))) 2)))))
  (t/testing "predicate fail"
    (do
      (syn/start)
      (syn/defpred 'pred '(-> (-> i o) i o))
      (t/is (u/ko-expr?
             (typ/elaborate-and-freevar-pred
              {} (deref syn/progpreds) '(pred (λ 1 F) F))))))
  (t/testing "used twice"
    (do
      (syn/start)
      (syn/defpred 'twice '(-> i i o))
      (t/is (u/ok-expr?
             (typ/elaborate-and-freevar-pred
              {} (deref syn/progpreds) '(twice ((λ 1 O) A) A)))))))

(t/deftest check-clause-test
  (t/testing "oddeven"
    (do
      (syn/start)
      (syn/defpred 'even '(-> i o))
      (syn/defpred 'odd '(-> i o))
      (t/is (= {'N 'i}
               (nth
                (typ/elaborate-and-freevar-clause
                 {} (deref syn/progpreds) '((even (S N)) ((odd N)))) 2)))))
  (t/testing "pred"
    (do
      (syn/start)
      (syn/defpred 'applypred '(-> (-> i o) i o))
      (t/is (= {'P '(-> i o) 'N 'i}
               (nth
                (typ/elaborate-and-freevar-clause
                 {} (deref syn/progpreds) '((applypred P N) ((P N)))) 2)))))
  (t/testing "incoherent"
    (do
      (syn/start)
      (syn/defpred 'even '(-> i o))
      (syn/defpred 'odd '(-> o o))
      (t/is (u/ko-expr?
             (typ/elaborate-and-freevar-clause
              {} (deref syn/progpreds) '((even (S N)) ((odd N)))))))))

(t/deftest elaborate-program-test
  (t/testing "oddeven"
    (do
      (syn/start)
      (syn/defpred 'even '(-> i o))
      (syn/defpred 'odd '(-> i o))
      (syn/addclause '((even O)))
      (syn/addclause '((even (S N)) :- (odd N)))
      (syn/addclause '((odd (S O))))
      (syn/addclause '((odd (S N)) :- (even N)))
      (t/is (= [:ok (deref syn/progpreds)]
               (typ/elaborate-program {} (deref syn/progpreds)))))))

(t/deftest valid-type?-test
  (t/testing "positive"
    (t/is (= :ok (typ/valid-type? '#{bool} 'bool)))
    (t/is (= :ok (typ/valid-type? '#{bool} 'i)))
    (t/is (= :ok (typ/valid-type? '#{bool} '(-> i i bool)))))
  (t/testing "negative"
    (t/is (u/ko-expr? (typ/valid-type? #{} '(-> i bool))))))

(t/deftest elaborate-and-check-program-test
  (t/testing "oddeven"
    (do
      (syn/start)
      (syn/defpred 'even '(-> i o))
      (syn/defpred 'odd '(-> i o))
      (syn/addclause '((even O)))
      (syn/addclause '((even (S N)) :- (odd N)))
      (syn/addclause '((odd (S O))))
      (syn/addclause '((odd (S N)) :- (even N)))
      (t/is (= [:ok (deref syn/progpreds)]
               (typ/elaborate-and-check-program #{} {} (deref syn/progpreds)))))))
