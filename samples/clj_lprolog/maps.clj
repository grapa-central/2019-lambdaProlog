(ns clj-lprolog.maps
  (:require [clj-lprolog.core :as lp]))

(load-file "samples/clj_lprolog/lists.clj")

;; Good old map function
(lp/defpred 'mapfun '(-> natlist (-> nat nat) natlist o))
(lp/addclause '((mapfun ni F ni)))
(lp/addclause '((mapfun (cs X L1) F (cs (F X) L2)) :- (mapfun L1 F L2)))

(lp/defpred 'reduce '(-> natlist (-> nat nat nat) nat nat o))
(lp/addclause '((reduce ni F X X)))
(lp/addclause '((reduce (cs W L) F X (F W Y)) :- (reduce L F X Y)))

;; For each
(lp/defpred 'foreach '(-> natlist (-> nat o) o))
(lp/addclause '((foreach ni P)))
(lp/addclause '((foreach (cs X L) P) :- (P X), (foreach L P)))

;; Map a predicate
(lp/defpred 'mappred '(-> natlist (-> nat nat o) natlist o))
(lp/addclause '((mappred ni P ni)))
(lp/addclause '((mappred (cs X L1) P (cs X L2)) :- (P X Y), (mappred L1 P L2)))

(lp/type-check-program?)
