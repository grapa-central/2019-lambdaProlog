(ns clj-lprolog.typecheck-test
  (:require [clj-lprolog.typecheck :as typ]
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

;; Not easy to test metadata simply...

(defn test-elab-meta
  [] (binding [*print-meta* true]
       (do (pr (typ/elaborate-term '((λ 2 (+ 1 1)) A O))) (println ""))))
