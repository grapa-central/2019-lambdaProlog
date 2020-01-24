(ns clj-lprolog.ndprover.formulas
  (:require [clj-lprolog.core :as lp]))

(load-file "samples/clj_lprolog/ndprover/logic.clj")
(load-file "samples/clj_lprolog/ndprover/nonlogical.clj")

(lp/deftype 'name)
(lp/defconst 'bugs 'name)
(lp/defconst 'baffler 'name)
(lp/defconst 'cases1 'name)

(lp/defpred 'formula '(-> name bool o))

(lp/addclause '((formula
                 bugs (imp
                       (and
                        (and
                         (and (heated jar)
                              (forall (λ [x]
                                         (imp (bug X) (animal X)))))
                         (forall (λ [x]
                                    (forall (λ [y] (imp (and (and (heated y) (in x y)) (animal x)) (dead x)))))))
                        (forall (λ [y]
                                   (imp
                                    (forall (λ [x]
                                            (imp (and (in x y) (bug X)) (dead x))))
                                    (sterile y)))))
                       (sterile jar)))))

(lp/addclause '((formula baffler (some (λ [x] (forall (λ [y] (imp (p x) (p y)))))))))

(lp/addclause '((formula cases1 (imp (or (q a) (q b)) (some (λ [x] (q x)))))))

(lp/type-check-program)
