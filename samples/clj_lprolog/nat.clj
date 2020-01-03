(ns clj-lprolog.oddeven
  (:require [clj-lprolog.core :as lp]))

(lp/start)

(lp/deftype 'nat)
(lp/defconst 'zero 'nat)
(lp/defconst 'succ '(-> nat nat))

;; Defining the even and odd predicates
;; (this could actually be done out of order)
(lp/defpred 'even '(-> nat o))
(lp/defpred 'odd '(-> nat o))

;; We need two rules for even
(lp/addclause '((even zero)))
(lp/addclause '((even (succ N)) :- (odd N)))

;; And only one for odd, using even
(lp/addclause '((odd (succ N)) :- (even N)))

(lp/type-check-program?)
