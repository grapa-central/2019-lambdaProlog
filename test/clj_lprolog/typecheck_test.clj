(ns clj-lprolog.typecheck-test
  (:require [clj-lprolog.utils :as u]
            [clj-lprolog.typecheck :as typ]
            [clj-lprolog.presyntax :as syn]
            [clj-lprolog.core :as cor]
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
    (t/is (= [:ok {'Ty1 'a}] (typ/mgu-ty 'a '(-> Ty1))))
    (t/is (= [:ok {'Ty1 'nat}]
             (typ/mgu-ty '(list nat) '(list Ty1))))
    (t/is (= '[:ok {Ty1 (-> goal goal o) Ty2 (-> goal goal o)}]
           (typ/mgu-ty '(-> (-> goal goal o) goal goal o) '(-> Ty1 Ty2)))))
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

(def tacconsts '{then (-> (-> goal goal o) (-> goal goal o) goal goal o)
                 repeattac (-> (-> goal goal o) goal goal o)})

(t/deftest infer-term-test
  (t/testing "positive"
    (t/is (u/ok-expr? (typ/subst-infer-term {} #{0} ['i] 0)))
    (t/is (= [:ok 'int] (typ/infer-term 42)))
    (t/is (= [:ok 'nat] (typ/infer-term {'zero 'nat} 'zero)))
    (t/is (= [:ok 'string] (typ/infer-term {} "hello")))
    (t/is (= [:ok 'int] (typ/infer-term {} 42)))
    (t/is (= [:ok 'nat] (typ/infer-term {'succ '(-> nat nat)} '(succ N))))
    (t/is (= [:ok '(-> int int)] (typ/infer-term '(+ (* 2 4)))))
    (t/is (u/ok-expr? (typ/infer-term '(λ 1 #{0}))))
    (t/is (u/ok-expr? (typ/infer-term '(λ 2 #{1}))))
    (t/is (u/ok-expr? (typ/infer-term '((λ 2 #{1}) O))))
    (t/is (= [:ok '(-> int int)] (typ/infer-term '((λ 2 (+ #{0} #{1})) 2))))
    (t/is (= [:ok 'int] (typ/infer-term '(λ 0 4))))
    (t/is (= [:ok '(-> goal goal o)] (typ/infer-term tacconsts '(repeattac Tac))))
    (t/is (= [:ok '(-> goal goal o)]
             (typ/infer-term tacconsts '(then Tac (repeattac Tac))))))
  (t/testing "negative"
    (t/is (u/ko-expr? (typ/infer-term '(12 12))))
    (t/is (u/ko-expr? (typ/infer-term '((λ 1 (+ #{0} #{0})) (+ 12)))))))

(t/deftest elaborate-term-test
  (t/testing "elaborated terms stay the same"
    (t/is (= '((λ 2 #{0}) A B)
             (second (typ/elaborate-term {} '((λ 2 #{0}) A B)))))))

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
    (t/is (= [:ok {'A 'o}] (elab-and-freevars '(λ 1 A) '(-> int o))))
    (t/is (= [:ok {'A 'int}] (elab-and-freevars '((λ 1 A) A) 'int)))
    (t/is (= [:ok {'A 'int 'B 'int}]
             (elab-and-freevars '((λ 2 (+ #{0} #{1})) A B) 'int))))
  (t/testing "negative"
    (t/is (u/ko-expr? (elab-and-freevars '((λ 1 (+ (A 0) #{0})) A) 'int)))))

(t/deftest check-pred-test
  (t/testing "even"
    (do
      (cor/start)
      (cor/defpred even (-> int o))
      (t/is (= '{A int}
               (nth
                (typ/elaborate-and-freevar-pred
                 {} (deref cor/progpreds) '(even A)) 2)))))
  (t/testing "predicate"
    (do
      (cor/start)
      (cor/defpred pred (-> (-> int o) int o))
      (t/is (= {'F 'o}
               (nth
                (typ/elaborate-and-freevar-pred
                 {} (deref cor/progpreds) '(pred (λ 1 F) 42)) 2)))))
  (t/testing "predicate fail"
    (do
      (cor/start)
      (cor/defpred pred (-> (-> int o) int o))
      (t/is (u/ko-expr?
             (typ/elaborate-and-freevar-pred
              {} (deref cor/progpreds) '(pred (λ 1 F) F))))))
  (t/testing "used twice"
    (do
      (cor/start)
      (cor/defpred twice (-> int int o))
      (t/is (u/ok-expr?
             (typ/elaborate-and-freevar-pred
              {} (deref cor/progpreds) '(twice ((λ 1 O) A) A)))))))

(t/deftest check-clause-test
  (t/testing "oddeven"
    (do
      (cor/start)
      (cor/defpred even (-> int o))
      (cor/defpred odd (-> int o))
      (t/is (= {'N 'int}
               (nth
                (typ/elaborate-and-freevar-clause
                 #{} '{succ (-> int int)} (deref cor/progpreds)
                 '((even (succ N)) ((print N) (odd N)))) 2)))))
  (t/testing "pred"
    (do
      (cor/start)
      (cor/defpred applypred (-> (-> int o) int o))
      (t/is (= {'P '(-> int o) 'N 'int}
               (nth
                (typ/elaborate-and-freevar-clause
                 {} {} (deref cor/progpreds) '((applypred P N) ((P N)))) 2)))))
  (t/testing "incoherent"
    (do
      (cor/start)
      (cor/defpred even (-> int o))
      (cor/defpred odd (-> o o))
      (t/is (u/ko-expr?
             (typ/elaborate-and-freevar-clause
              {} {} (deref cor/progpreds) '((even N) ((odd N)))))))))

(t/deftest elaborate-program-test
  (t/testing "oddeven"
    (do
      (cor/start)
      (cor/defpred even (-> int o))
      (cor/defpred odd (-> int o))
      (cor/defconst succ (-> int int))
      (cor/addclause (even 0))
      (cor/addclause (even (succ N)) :- (odd N))
      (cor/addclause (odd 1))
      (cor/addclause (odd (succ N)) :- (even N))
      (t/is (= [:ok (deref cor/progpreds)]
               (typ/elaborate-program {} (deref cor/progconsts)
                                      (deref cor/progpreds)))))))

(t/deftest valid-type?-test
  (t/testing "positive"
    (t/is (= :ok (typ/valid-type? '#{bool} 'bool)))
    (t/is (= :ok (typ/valid-type? '#{bool} 'int)))
    (t/is (= :ok (typ/valid-type? '#{bool} '(-> int int bool))))
    (t/is (= :ok (typ/valid-type? '#{} '(-> string o)))))
  (t/testing "negative"
    (t/is (u/ko-expr? (typ/valid-type? #{} '(-> i bool))))))

(t/deftest elaborate-and-check-program-test
  (t/testing "oddeven"
    (do
      (cor/start)
      (cor/defconst succ (-> int int))
      (cor/defpred even (-> int o))
      (cor/defpred odd (-> int o))
      (cor/addclause (even 0))
      (cor/addclause (even (succ N)) :- (odd N))
      (cor/addclause (odd (succ 0)))
      (cor/addclause (odd (succ N)) :- (even N))
      (t/is (= [:ok (deref cor/progpreds)]
               (typ/elaborate-and-check-program #{} '{succ (-> int int)}
                                                (deref cor/progpreds)))))))
