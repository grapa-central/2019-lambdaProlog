(ns clj-lprolog.stlc
  (:require [clj-lprolog.core :as lp]))

;; Simply-Typed-Lambda-Calculus

;; The terms are based on λProlog user terms
(lp/deftype 'term)
(lp/defconst 'tr 'term)
(lp/defconst 'fl 'term)
(lp/defconst 'abs '(-> (-> term term) term))
(lp/defconst 'app '(-> term term term))

;; The type syntax
(lp/deftype 'typ)
(lp/defconst 'bool 'typ)
(lp/defconst 'arr '(-> typ typ typ))

;; Evaluation
(lp/defpred 'eval '(-> term term o))
(lp/addclause '((eval tr tr)))
(lp/addclause '((eval fl fl)))
(lp/addclause '((eval (abs R) (abs R))))
(lp/addclause '((eval (app M N) V) :- (eval M (abs R)) (eval N U) (eval (R U) V)))

;; Some evaluation examples (from the TAS course)
;; This evaluator does not reduce under abstrations, so the results are not "tidy"
(def K '(abs (λ [x] (abs (λ [y] x)))))
(def S '(abs (λ [x] (abs (λ [y] (abs (λ [z] (app (app x z) (app y z)))))))))
(def I '(abs (λ [x] x)))
(def KII (list 'app (list 'app K I) I))
(def SKK (list 'app (list 'app S K) K))
(def DELTA '(abs (λ [x] (app x x))))

;; SKK is the Identity
(lp/solve (list 'eval (list 'app SKK 'fl) 'V))
;; (DELTA DELTA) loops infinetely
;;(lp/solve (list 'eval (list 'app DELTA DELTA) 'V))

;; Type inference
(lp/defpred 'infer '(-> term typ o))
(lp/addclause '((infer tr bool)))
(lp/addclause '((infer fl bool)))
(lp/addclause '((infer (app M N) B) :- (infer M (arr A B)) (infer N A)))
(lp/addclause '((infer (abs M) (arr A B)) :-
                (Π (x :> term) (=> (infer x A) (infer (M x) B)))))

;; Some typing examples
(lp/solve '(infer (abs (λ [x] x)) Ty))
(lp/solve (list 'infer KII 'Ty)) ;; KII is the identity
(lp/solve (list 'infer SKK 'Ty)) ;; SKK is also the identity
(lp/solve (list 'infer (list 'app DELTA DELTA) 'Ty))
