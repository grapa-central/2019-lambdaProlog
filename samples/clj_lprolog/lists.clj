(ns clj-lprolog.lists
  (:require [clj-lprolog.core :as lp]))

;; We will need the nats for future requests
(load-file "samples/clj_lprolog/nat.clj")

;; Generic lists
(lp/deftype (list A))
(lp/defconst ni (list A))
(lp/defconst cs (-> A (list A) (list A)))

;; list equality
(lp/defpred id (-> (list A) (list A) o))
(lp/addclause (id ni ni))
(lp/addclause (id (cs X L1) (cs X L2)) :- (id L1 L2))

;; membership of a list
(lp/defpred member (-> A (list A) o))
(lp/addclause (member X (cs X L)))
(lp/addclause (member X (cs Y L)) :- (member X L))

;; append
(lp/defpred append (-> (list A) (list A) (list A) o))
(lp/addclause (append ni L L))
(lp/addclause (append (cs X L1) L2 (cs X L3)) :- (append L1 L2 L3))

;; reverse a list
(lp/defpred reverse (-> (list A) (list A) o))
(lp/addclause (reverse ni ni))
(lp/addclause (reverse (cs X L1) L2) :-
                (reverse L1 L3), (append L3 (cs X ni) L2))

;; Some requests
(lp/solve (id (cs X (cs Y ni)) L)) ;; => [:ok ...]
(lp/solve (member zero (cs Y (cs X ni)))) ;; => [:ok {Y zero}]
(lp/solve (member (succ zero) (cs zero (cs zero ni)))) ;; => [:ko ...]
(lp/solve (append (cs zero ni) L (cs zero (cs (succ zero) ni)))) ;; => [:ok ...])
(lp/solve (append (cs zero ni) L L)) ;; => [:ko ...]
(lp/solve (reverse (cs (succ zero) (cs zero ni)) L)) ;; => [:ok ...]
(lp/solve (reverse (cs (succ zero) (cs zero ni))
                   (cs (succ zero) (cs zero ni)))) ;; => [:ko ...]
