(ns clj-lprolog.stlc
  (:require [clj-lprolog.core :as lp]))

;; Simply-Typed-Lambda-Calculus
;; The more complete PCF language is available in the pcf/ folder

;; The terms are based on λProlog user terms
(lp/deftype term)
(lp/defconst tr term)
(lp/defconst fl term)
(lp/defconst abs (-> (-> term term) term))
(lp/defconst app (-> term term term))

;; The type syntax
(lp/deftype typ)
(lp/defconst bool typ)
(lp/defconst arr (-> typ typ typ))

;; Evaluation
(lp/defpred eval (-> term term o))
(lp/addclause (eval tr tr))
(lp/addclause (eval fl fl))
(lp/addclause (eval (abs R) (abs R)))
(lp/addclause (eval (app M N) V) :- (eval M (abs R)) (eval N U) (eval (R U) V))

;; Type inference
(lp/defpred infer (-> term typ o))
(lp/addclause (infer tr bool))
(lp/addclause (infer fl bool))
(lp/addclause (infer (app M N) B) :- (infer M (arr A B)) (infer N A))
(lp/addclause (infer (abs M) (arr A B)) :-
(Π (x :> term) (=> (infer x A) (infer (M x) B))))

;; A typing example
(lp/solve (infer (abs (λ [x] x)) Ty))
