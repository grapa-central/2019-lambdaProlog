(ns clj-lprolog.typecheck-test
  (:require [clj-lprolog.typecheck :as typ]
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
    (t/is (= {'ty1 'A} (typ/mgu-ty 'ty1 'A)))
    (t/is (= {'ty1 'A 'ty2 'A}
             (typ/mgu-ty '(-> ty1 (-> ty1 ty2)) '(-> ty1 (-> A A)))))
    (t/is (= {'ty1 'A 'ty2 'A}
             (typ/mgu-ty '(-> ty1 (-> ty1 ty2)) '(-> A (-> ty1 A)))))
    (t/is (= {'ty1 'A 'ty2 '(-> B C)}
             (typ/mgu-ty '(-> A B C) '(-> ty1 ty2) ))))
  (t/testing "negative"
    (t/is (nil? (typ/mgu-ty 'A 'B)))
    (t/is (nil? (typ/mgu-ty '(-> ty1 ty1) '(-> A B))))
    (t/is (nil? (typ/mgu-ty '(-> ty1 ty2 ty2) '(-> A ty1 B))))
    (t/is (nil? (typ/mgu-ty 'ty1 '(-> ty1 ty2))))))

(t/deftest infer-term-test
  (t/testing "positive"
    (t/is (some? (typ/infer-term 0 ['i])))
    (t/is (some? (typ/infer-term '(S (S (S O))))))
    (t/is (= '(-> i i) (typ/infer-term '(+ (* (S O) (S O))))))
    (t/is (some? (typ/infer-term '(λ 1 0))))
    (t/is (some? (typ/infer-term '(λ 2 1))))
    (t/is (some? (typ/infer-term '('(λ 2 1) O))))
    (t/is (= '(-> i i) (typ/infer-term '((λ 2 (+ 0 1)) O)))))
  (t/testing "negative"
    (t/is (nil? (typ/infer-term '(S S))))
    (t/is (nil? (typ/infer-term '((λ 1 (+ 0 0)) S))))))

(t/deftest check-and-elaborate-term-test
  (t/testing "positive"
    (t/is (some? (typ/check-and-elaborate-term '(S (S (S O))) 'i)))
    (t/is (some? (typ/check-and-elaborate-term '(λ 1 0) '(-> A A))))
    (t/is (some? (typ/check-and-elaborate-term '(λ 2 1) '(-> A i A))))
    (t/is (some? (typ/check-and-elaborate-term '('(λ 2 1) O) '(-> A i)))))
  (t/testing "negative"
    (t/is (nil? (typ/check-and-elaborate-term '(λ 1 0) '(-> A B))))))

;; Not easy to test metadata simply...

(defn test-elab-meta
  [t] (binding [*print-meta* true]
       (do (pr (typ/elaborate-term t)) (println ""))))

;; (test-elab-meta '((λ 2 (+ 1 1)) A O))
;; (test-check-elab-meta '(A O B) 'i)

(defn test-check-elab-meta
  [t ty] (binding [*print-meta* true]
        (do (pr (typ/check-and-elaborate-term t ty)))))

(defn elab-and-freevars
  [t ty] (typ/get-freevar-types (typ/check-and-elaborate-term t ty)))

(t/deftest get-freevar-types-test
  (t/testing "positive"
    (t/is (= {'A 'o} (elab-and-freevars '(λ 1 A) '(-> i o))))
    (t/is (= {'A 'i} (elab-and-freevars '((λ 1 A) A) 'i)))
    (t/is (= {'A 'i 'B 'i} (elab-and-freevars '((λ 2 (+ 0 1)) A B) 'i)))
    (let [e (elab-and-freevars '(A O B) 'i) tB (get e 'B)]
      (t/is (= {'A (list '-> 'i tB 'i) 'B tB} e))))
  (t/testing "negative"
    (t/is (nil? (elab-and-freevars '((λ 1 (+ (A O) 0)) A) 'i)))))

(t/deftest check-and-freevar-types-pred
  (t/testing "even"
    (do
      (swap! syn/progpreds (fn [_] {}))
      (syn/defpred 'even '(-> i o))
      (t/is (= {'A 'i}
               (typ/check-and-freevar-types-pred '(even A)
                                                 (deref syn/progpreds))))))
  (t/testing "predicate"
    (do
      (swap! syn/progpreds (fn [_] {}))
      (syn/defpred 'pred '(-> (-> i o) i o))
      (t/is (= {'F 'o}
               (typ/check-and-freevar-types-pred '(pred (λ 1 F) (S O))
                                                 (deref syn/progpreds))))))
  (t/testing "predicate fail"
    (do
      (swap! syn/progpreds (fn [_] {}))
      (syn/defpred 'pred '(-> (-> i o) i o))
      (t/is (nil?
             (typ/check-and-freevar-types-pred '(pred (λ 1 F) F)
                                               (deref syn/progpreds))))))
  (t/testing "used twice"
    (do
      (swap! syn/progpreds (fn [_] {}))
      (syn/defpred 'twice '(-> i i o))
      (t/is (some?
             (typ/check-and-freevar-types-pred '(twice ((λ 1 O) A) A)
                                               (deref syn/progpreds)))))))
