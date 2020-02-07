(ns clj-lprolog.maps
  (:require [clj-lprolog.core :as lp]))

(load-file "samples/clj_lprolog/lists.clj")

;; Good old map function
(lp/defpred mapfun (-> (list A) (-> A B) (list B) o))
(lp/addclause (mapfun ni F ni))
(lp/addclause (mapfun (cs X L1) F (cs (F X) L2)) :- (mapfun L1 F L2))

;; Reduce
(lp/defpred reduce (-> (list A) (-> A B B) B B o))
(lp/addclause (reduce ni F X X))
(lp/addclause (reduce (cs W L) F X (F W Y)) :- (reduce L F X Y))

;; For each
(lp/defpred foreach (-> (list A) (-> A o) o))
(lp/addclause (foreach ni P))
(lp/addclause (foreach (cs X L) P) :- (P X), (foreach L P))

;; Map a predicate
(lp/defpred mappred (-> (list A) (-> A B o) (list B) o))
(lp/addclause (mappred ni P ni))
(lp/addclause (mappred (cs X L1) P (cs Y L2)) :- (P X Y), (mappred L1 P L2))

(lp/solve (mapfun (cs zero ni) (λ [x] (succ x)) L))
(lp/solve (mapfun L (λ [x] (succ x)) (cs (succ zero) ni)))
(lp/solve (mapfun (cs zero ni) F (cs (succ zero) ni)))
(lp/solve (foreach (cs zero (cs (succ X) ni)) even))
