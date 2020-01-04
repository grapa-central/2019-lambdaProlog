(ns clj-lprolog.lists
  (:require [clj-lprolog.core :as lp]))

(load-file "samples/clj_lprolog/nat.clj")

;; Lists of natural numbers
(lp/deftype 'natlist)
(lp/defconst 'ni 'natlist)
(lp/defconst 'cs '(-> nat natlist natlist))

;; list equality
(lp/defpred 'id '(-> natlist natlist o))
(lp/addclause '((id ni ni)))
(lp/addclause '((id (cs X L1) (cs X L2)) :- (id L1 L2)))

;; membership of a list
(lp/defpred 'member '(-> nat natlist o))
(lp/addclause '((member X (cs X L))))
(lp/addclause '((member X (cs Y L)) :- (member X L)))

;; append
(lp/defpred 'append '(-> natlist natlist natlist o))
(lp/addclause '((append ni L L)))
(lp/addclause '((append (cs X L1) L2 (cs X L3)) :- (append L1 L2 L3)))

;; reverse a list
(lp/defpred 'reverse '(-> natlist natlist o))
(lp/addclause '((reverse ni ni)))
(lp/addclause '((reverse (cs X L1) L2) :- (reverse L1 L3), (append L3 (cs X ni) L2)))

(lp/type-check-program?)
