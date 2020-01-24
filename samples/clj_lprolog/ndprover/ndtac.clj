(ns clj-lprolog.ndprover.ndtac
  (:require [clj-lprolog.core :as lp]))

;; (lp/start)

(load-file "samples/clj_lprolog/ndprover/listmanip.clj")
(load-file "samples/clj_lprolog/ndprover/goaltypes.clj")
(load-file "samples/clj_lprolog/ndprover/logic.clj")
(load-file "samples/clj_lprolog/ndprover/ndproofs.clj")

(lp/deftype 'judgment)
(lp/deftype 'answer)

(lp/defconst 'of-type '(-> proof-object bool judgment))
(lp/defconst '--> '(-> (list judgment) judgment goal))

(lp/defconst 'yes 'answer)


(lp/defpred 'exists-e-tac '(-> nat goal goal o))
(lp/defpred 'or-e-tac '(-> nat goal goal o))
(lp/defpred 'forall-e-query '(-> nat goal goal o))
(lp/defpred 'forall-e-tac '(-> nat goal goal o))
(lp/defpred 'fchain-tac '(-> nat goal goal o))
(lp/defpred 'bchain-tac '(-> nat goal goal o))
(lp/defpred 'imp-e-retain '(-> nat goal goal o))
(lp/defpred 'imp-e-tac '(-> nat goal goal o))
(lp/defpred 'and-e-tac '(-> nat goal goal o))
(lp/defpred 'exists-i-query '(-> goal goal o))
(lp/defpred 'exists-i-tac '(-> goal goal o))
(lp/defpred 'forall-i-tac '(-> goal goal o))
(lp/defpred 'imp-i-tac '(-> goal goal o))
(lp/defpred 'or-i2-tac '(-> goal goal o))
(lp/defpred 'or-i1-tac '(-> goal goal o))
(lp/defpred 'and-i-tac '(-> goal goal o))
(lp/defpred 'close-tacn '(-> nat goal goal o))
(lp/defpred 'close-tac '(-> goal goal o))


(lp/addclause '((close-tac (--> Gamma (of-type P A)) truegoal) :- (member (of-type P A) Gamma)))

(lp/addclause '((close-tacn N (--> Gamma (of-type P A)) truegoal) :- (nth-item N (of-type P A) Gamma)))

(lp/addclause '((and-i-tac (--> Gamma (of-type (and-i P1 P2) (and A B)))
                           (andgoal (--> Gamma (of-type P1 A)) (--> Gamma (of-type P2 B))))))

(lp/addclause '((or-i1-tac (--> Gamma (of-type (or-i1 P) (or A B)))
                           (--> Gamma (of-type P A)))))

(lp/addclause '((or-i2-tac (--> Gamma (of-type (or-i2 P) (or A B)))
                           (--> Gamma (of-type P B)))))

(lp/addclause '((imp-i-tac (--> Gamma (of-type (imp-i P) (imp A B)))
                           (allgoal (λ [pa] (--> (cs (of-type pa A) Gamma)
                                                  (of-type (P pa) B)))))))

(lp/addclause '((forall-i-tac (--> Gamma (of-type (forall-i P) (forall A)))
                              (allgoal (λ [t] (--> Gamma (of-type (P t) (A t))))))))

(lp/addclause '((exists-i-tac (--> Gamma (of-type (exists-i T P) (some A)))
                              (--> Gamma (of-type P (A T))))))


;; NEED PRINT AND READ
;; (lp/addclause '((exists-i-query (--> Gamma (of-type (exists-i T P) (some A)))
;;                                 (--> Gamma (of-type P (A T)))) :-
;;                 (print "Enter substitution term: "), (read T)))

(lp/addclause '((and-e-tac N
                           (--> Gamma (of-type Pc C))
                           (--> (cs (of-type (and-e1 P) A) (cs (of-type (and-e2 P) B) Gamma1))
                                (of-type Pc C))) :-
                (nth-item-and-rest N (of-type P (and A B)) Gamma Gamma1)))

(lp/addclause '((imp-e-tac N
                           (--> Gamma (of-type Pc C))
                           (andgoal (--> Gamma1 (of-type Pa A))
                                    (--> (cs (of-type (imp-e Pa P) B) Gamma1) (of-type Pc C)))) :-
                (nth-item-and-rest N (of-type P (imp A B)) Gamma Gamma1)))

(lp/addclause '((imp-e-retain N
                              (--> Gamma (of-type Pc C))
                              (andgoal (--> Gamma (of-type Pa A))
                                       (--> (cs (of-type (imp-e Pa P) B) Gamma) (of-type Pc C)))) :-
                (nth-item N (of-type P (imp A B)) Gamma)))

(lp/addclause '((bchain-tac N
                            (--> Gamma (of-type (imp-e Pa P) B))
                            (--> Gamma1 (of-type Pa A))) :-
                (nth-item-and-rest N (of-type P (imp A B)) Gamma Gamma1)))

(lp/addclause '((fchain-tac N
                            (--> Gamma (of-type Pc C))
                            (--> (cs (of-type (imp-e Pa P) B) Gamma2) (of-type Pc C))) :-
                (nth-item-and-rest N (of-type P (imp A B)) Gamma Gamma1),
                (member-and-rest (of-type Pa A) Gamma1 Gamma2)))

(lp/addclause '((forall-e-tac N
                              (--> Gamma (of-type Pc C))
                              (--> (cs (of-type (forall-e T P) (A T)) Gamma1) (of-type Pc C))) :-
                (nth-item-and-rest N (of-type P (forall A)) Gamma Gamma1)))

;; NEED PRIND AND READ
;; (lp/addclause '((forall-e-query N
;;                                 (--> Gamma (of-type Pc C))
;;                                 (--> (cs (of-type (forall-e T P) (A T)) Gamma1) (of-type Pc C))) :-
;;                 (print "Enter substitution term: "),
;;                 (read T),
;;                 (print "Remove hypothesis? "),
;;                 ((read yes), (nth-item-and-rest N (of-type P (forall A)) Gamma Gamma1);
;;                  (Gamma1 = Gamma), (nth-item N (of-type P (forall A)) Gamma))))

(lp/addclause '((or-e-tac N
                          (--> Gamma (of-type (or-e P P1 P2) C))
                          (andgoal (allgoal (λ [pa] (--> (cs (of-type pa A) Gamma1)
                                                         (of-type (P1 pa) C))))
                                   (allgoal (λ [pb] (--> (cs (of-type pb B) Gamma1)
                                                         (of-type (P2 pb) C))))))))

(lp/addclause '((exists-e-tac N
                              (--> Gamma (of-type (exists-e P Pc) C))
                              (allgoal (λ [t]
                                          (allgoal (λ [pa]
                                                      (--> (cs (of-type pa (A t)) Gamma1)
                                                           (of-type (Pc t pa) C))))))) :-
                (nth-item-and-rest N (of-type P (some A)) Gamma Gamma1)))

;; (lp/type-check-program)
