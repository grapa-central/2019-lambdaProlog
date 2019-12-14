(ns clj-lprolog.typecheck-test
  (:require [clj-lprolog.utils :as u]
            [clj-lprolog.typecheck :as typ]
            [clj-lprolog.presyntax :as syn]
            [clojure.test :as t]))

(t/deftest substitute-ty-test
  (t/is (= 'S (typ/substitute-ty 'ty1 'S 'ty1)))
  (t/is (= '(-> ty2 S o) (typ/substitute-ty 'ty1 'S '(-> ty2 ty1 o)))))

(t/deftest apply-subst-ty-test
  (t/is (= '(-> X Y) (typ/apply-subst-ty {'ty1 'X 'ty2 'Y} '(-> ty1 ty2))))
  (t/is (= '(-> X ty2) (typ/apply-subst-ty {'ty1 'ty3 'ty3 'X} '(-> ty1 ty2)))))

(t/deftest mgu-ty-test
  (t/testing "positive"
    (t/is (= [:ok {'ty1 'A}] (typ/mgu-ty 'ty1 'A)))
    (t/is (= [:ok {'ty1 'A 'ty2 'A}]
             (typ/mgu-ty '(-> ty1 (-> ty1 ty2)) '(-> ty1 (-> A A)))))
    (t/is (= [:ok {'ty1 'A 'ty2 'A}]
             (typ/mgu-ty '(-> ty1 (-> ty1 ty2)) '(-> A (-> ty1 A)))))
    (t/is (= [:ok {'ty1 'A 'ty2 '(-> B C)}]
             (typ/mgu-ty '(-> A B C) '(-> ty1 ty2) ))))
  (t/testing "negative"
    (t/is (u/ko-expr? (typ/mgu-ty 'A 'B)))
    (t/is (u/ko-expr? (typ/mgu-ty '(-> ty1 ty1) '(-> A B))))
    (t/is (u/ko-expr? (typ/mgu-ty '(-> ty1 ty2 ty2) '(-> A ty1 B))))
    (t/is (u/ko-expr? (typ/mgu-ty 'ty1 '(-> ty1 ty2))))))

(t/deftest infer-term-test
  (t/testing "positive"
    (t/is (u/ok-expr? (typ/subst-infer-term 0 ['i] 0)))
    (t/is (= [:ok 'i] (typ/infer-term '(S (S (S O))))))
    (t/is (= [:ok '(-> i i)] (typ/infer-term '(+ (* (S O) (S O))))))
    (t/is (u/ok-expr? (typ/infer-term '(λ 1 0))))
    (t/is (u/ok-expr? (typ/infer-term '(λ 2 1))))
    (t/is (u/ok-expr? (typ/infer-term '((λ 2 1) O))))
    (t/is (= [:ok '(-> i i)] (typ/infer-term '((λ 2 (+ 0 1)) O))))
    (t/is (= [:ok 'i] (typ/infer-term '(λ 0 (S O))))))
  (t/testing "negative"
    (t/is (u/ko-expr? (typ/infer-term '(S S))))
    (t/is (u/ko-expr? (typ/infer-term '((λ 1 (+ 0 0)) S))))))

(t/deftest check-and-elaborate-term-test
  (t/testing "positive"
    (t/is (u/ok-expr? (typ/check-and-elaborate-term '(S (S (S O))) 'i)))
    (t/is (u/ok-expr? (typ/check-and-elaborate-term '(λ 1 0) '(-> A A))))
    (t/is (u/ok-expr? (typ/check-and-elaborate-term '(λ 2 1) '(-> A i A))))
    (t/is (u/ok-expr? (typ/check-and-elaborate-term '((λ 2 1) O) '(-> A i)))))
  (t/testing "negative"
    (t/is (u/ko-expr? (typ/check-and-elaborate-term '(λ 1 0) '(-> A B))))))

;; Not easy to test metadata simply...

(defn test-elab-meta
  [t] (binding [*print-meta* true]
       (do (pr (typ/elaborate-term t)) (println ""))))

;; (test-elab-meta '(λ 0 O))
;; (test-check-elab-meta '(A O B) 'i)

(defn test-check-elab-meta
  [t ty] (binding [*print-meta* true]
        (do (pr (typ/check-and-elaborate-term t ty)))))

(defn elab-and-freevars
  [t ty] (u/ok> (typ/check-and-elaborate-term t ty) :as [_ t]
                (typ/get-freevar-types t)))

(elab-and-freevars '(λ 1 A) '(-> i o))
(typ/check-and-elaborate-term '((λ 1 A) A) 'i)

(t/deftest get-freevar-types-test
  (t/testing "positive"
    (t/is (= [:ok {'A 'o}] (elab-and-freevars '(λ 1 A) '(-> i o))))
    (t/is (= [:ok {'A 'i}] (elab-and-freevars '((λ 1 A) A) 'i)))
    (t/is (= [:ok {'A 'i 'B 'i}] (elab-and-freevars '((λ 2 (+ 0 1)) A B) 'i))))
  (t/testing "negative"
    (t/is (u/ko-expr? (elab-and-freevars '((λ 1 (+ (A O) 0)) A) 'i)))))

(t/deftest check-pred-test
  (t/testing "even"
    (do
      (swap! syn/progpreds (fn [_] {}))
      (syn/defpred 'even '(-> i o))
      (t/is (= {'A 'i}
               (nth
                (typ/elaborate-and-freevar-pred '(even A)
                                                (deref syn/progpreds)) 2)))))
  (t/testing "predicate"
    (do
      (swap! syn/progpreds (fn [_] {}))
      (syn/defpred 'pred '(-> (-> i o) i o))
      (t/is (= {'F 'o}
               (nth
                (typ/elaborate-and-freevar-pred '(pred (λ 1 F) (S O))
                                                (deref syn/progpreds)) 2)))))
  (t/testing "predicate fail"
    (do
      (swap! syn/progpreds (fn [_] {}))
      (syn/defpred 'pred '(-> (-> i o) i o))
      (t/is (u/ko-expr?
             (typ/elaborate-and-freevar-pred '(pred (λ 1 F) F)
                                             (deref syn/progpreds))))))
  (t/testing "used twice"
    (do
      (swap! syn/progpreds (fn [_] {}))
      (syn/defpred 'twice '(-> i i o))
      (t/is (u/ok-expr?
             (typ/elaborate-and-freevar-pred '(twice ((λ 1 O) A) A)
                                             (deref syn/progpreds)))))))

(t/deftest check-clause-test
  (t/testing "oddeven"
    (do
      (swap! syn/progpreds (fn [_] {}))
      (syn/defpred 'even '(-> i o))
      (syn/defpred 'odd '(-> i o))
      (t/is (= {'N 'i}
               (nth
                (typ/elaborate-and-freevar-clause '((even (S N)) ((odd N)))
                                                  (deref syn/progpreds)) 2)))))
  (t/testing "incoherent"
    (do
      (swap! syn/progpreds (fn [_] {}))
      (syn/defpred 'even '(-> i o))
      (syn/defpred 'odd '(-> o o))
      (t/is (u/ko-expr?
             (typ/elaborate-and-freevar-clause '((even (S N)) ((odd N)))
                                               (deref syn/progpreds)))))))

(t/deftest elaborate-program-test
  (t/testing "oddeven"
    (do
      (swap! syn/progpreds (fn [_] {}))
      (syn/defpred 'even '(-> i o))
      (syn/defpred 'odd '(-> i o))
      (syn/addclause '((even O)))
      (syn/addclause '((even (S N)) :- (odd N)))
      (syn/addclause '((odd (S O))))
      (syn/addclause '((odd (S N)) :- (even N)))
      (t/is (= [:ok (deref syn/progpreds)]
               (typ/elaborate-program (deref syn/progpreds)))))))

(t/deftest elaborate-and-check-program-test
  (t/testing "oddeven"
    (do
      (swap! syn/progpreds (fn [_] {}))
      (syn/defpred 'even '(-> i o))
      (syn/defpred 'odd '(-> i o))
      (syn/addclause '((even O)))
      (syn/addclause '((even (S N)) :- (odd N)))
      (syn/addclause '((odd (S O))))
      (syn/addclause '((odd (S N)) :- (even N)))
      (t/is (= [:ok (deref syn/progpreds)]
               (typ/elaborate-and-check-program (deref syn/progpreds)))))))
