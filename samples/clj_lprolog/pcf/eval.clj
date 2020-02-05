(ns clj-lprolog.pcf.eval
  (:require [clj-lprolog.core :as lp]))

(load-file "samples/clj_lprolog/pcf/pcf.clj")

;;{
;; Call-by-value interpreter for PCF
;;}

(lp/defpred 'is '(-> tm tm o))
(lp/defpred 'eval '(-> tm tm o))

(lp/addclause '((is X X)))

(lp/addclause '((eval (abs R) (abs R))))
(lp/addclause '((eval (app M N) V) :- (eval M (abs R)) (eval (R N) V)))
(lp/addclause '((eval (fixpt R) V) :- (eval (R (fixpt R)) V)))
(lp/addclause '((eval (cond C T E) V) :-
                (eval C X) (is X (ib true)) (eval T V)))
(lp/addclause '((eval (cond C T E) V) :-
                (eval C X) (is X (ib false)) (eval E V)))

;; Bool primitives
(lp/addclause '((eval (ib B) (ib B))))
(lp/addclause '((eval (app (app && B1) B2) (ib (and V1 V2))) :-
                (eval B1 (ib V1)) (eval B2 (ib V2))))
(lp/addclause '((eval (app (app || B1) B2) (ib (or V1 V2))) :-
                (eval B1 (ib V1)) (eval B2 (ib V2))))

;; Num primitives
(lp/addclause '((eval (in N) (in N))))
(lp/addclause '((eval (app (app plus N1) N2) (in (+ V1 V2))) :-
                (eval N1 (in V1)) (eval N2 (in V2))))
(lp/addclause '((eval (app (app minus N1) N2) (in (- V1 V2))) :-
                (eval N1 (in V1)) (eval N2 (in V2))))
(lp/addclause '((eval (app (app times N1) N2) (in (* V1 V2))) :-
                (eval N1 (in V1)) (eval N2 (in V2))))
(lp/addclause '((eval (app (app equal E1) E2) (ib (= V1 V2))) :-
                (eval E1 (in V1)) (eval E2 (in V2))))
(lp/addclause '((eval (app (app greater E1) E2) (ib (> V1 V2))) :-
                (eval E1 (in V1)) (eval E2 (in V2))))
(lp/addclause '((eval (app zerop N1) (ib (zero? V1))) :-
                (eval N1 (in V1))))

;; List primitives
(lp/addclause '((eval ni ni)))
(lp/addclause '((eval (app (app cs E1) E2) (app (app cs V1) V2)) :-
                (eval E1 V1) (eval E2 V2)))
(lp/addclause '((eval (app car E) X) :- (eval E (app (app cs X) Y))))
(lp/addclause '((eval (app cdr E) Y) :- (eval E (app (app cs X) Y))))
(lp/addclause '((eval (app nullp E) (ib true)) :- (eval E ni)))
(lp/addclause '((eval (app nullp E) (ib false)) :- (eval E (app (app cs X) Y))))
(lp/addclause '((eval (app consp E) (ib false)) :- (eval E ni)))
(lp/addclause '((eval (app consp E) (ib true)) :- (eval E (app (app cs X) Y))))
