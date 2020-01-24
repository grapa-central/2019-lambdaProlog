(ns clj-lprolog.stlc
  (:require [clj-lprolog.core :as lp]))

;; Simply-Typed-Lambda-Calculus

;; The terms are based on λProlog user terms
(lp/deftype 'term)
(lp/defconst 'tr 'term)
(lp/defconst 'fl 'term)
(lp/defconst 'abs '(-> (-> term term) term))
(lp/defconst 'app '(-> term term term))

;; Evaluation
(lp/defpred 'eval '(-> term term o))
(lp/addclause '((eval tr tr)))
(lp/addclause '((eval fl fl)))
(lp/addclause '((eval (abs R) (abs R))))
(lp/addclause '((eval (app M N) V) :- (eval M (abs R)) (eval N U) (eval (R U) V)))

;; Some evaluation examples (from the TAS course)
;; This evaluator does not reduce under abstrations, so the results are not "tidy"
(def DELTA '(abs (λ [x] (app x x))))
(def K '(abs (λ [x] (abs (λ [y] x)))))
(def S '(abs (λ [x] (abs (λ [y] (abs (λ [z] (app (app x z) (app y z)))))))))

;; SKK is the Identity
(lp/solve (list 'eval (list 'app (list 'app (list 'app S K) K) 'fl) 'V))
;; (DELTA DELTA) loops
;;(lp/solve (list 'eval (list 'app DELTA DELTA) 'V))
