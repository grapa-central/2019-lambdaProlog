(ns clj-lprolog.pcf.monoinfer
  (:require [clj-lprolog.core :as lp]))

(load-file "samples/clj_lprolog/pcf/pcf.clj")
(load-file "samples/clj_lprolog/pcf/monotypes.clj")

;;{
;; Inferring monotypes for programs
;;}

(lp/defpred 'prim '(-> tm ty o)) ;; Type of a primitive
(lp/defpred 'infer '(-> tm ty o)) ;; Type of a term

(lp/addclause '((infer (app M N) B) :- (infer M (--> A B)) (infer N A)))
(lp/addclause '((infer (abs M) (--> A B)) :-
                (Π (x :> tm) (=> (infer x A) (infer (M x) B)))))
(lp/addclause '((infer (fixpt M) A) :-
                (Π (x :> tm) (=> (infer x A) (infer (M x) A)))))
(lp/addclause '((infer (cond C T E) A) :-
                (infer C bool) (infer T A) (infer E A)))
(lp/addclause '((infer T A) :- (prim T A)))

;; Boolean primitives
(lp/addclause '((prim (ib B) bool)))
(lp/addclause '((prim && (--> bool (--> bool bool)))))
(lp/addclause '((prim || (--> bool (--> bool bool)))))

;; Integer primitives
(lp/addclause '((prim (in N) num)))
(lp/addclause '((prim plus (--> num (--> num num)))))
(lp/addclause '((prim minus (--> num (--> num num)))))
(lp/addclause '((prim times (--> num (--> num num)))))
(lp/addclause '((prim equal (--> num (--> num bool)))))
(lp/addclause '((prim greater (--> num (--> num bool)))))
(lp/addclause '((prim zerop (--> num bool))))

;; List primitives
(lp/addclause '((prim ni (lst A))))
(lp/addclause '((prim cs (--> A (--> (lst A) (lst A))))))
(lp/addclause '((prim car (--> (lst A) A))))
(lp/addclause '((prim cdr (--> (lst A) (lst A)))))
(lp/addclause '((prim nullp (--> (lst A) bool))))
(lp/addclause '((prim consp (--> (lst A) bool))))
