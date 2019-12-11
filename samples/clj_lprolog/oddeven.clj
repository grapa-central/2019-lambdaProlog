(ns clj-lprolog.oddeven
  (:require [clj-lprolog.core :as lp]))

(lp/start)

;; Defining the even and odd predicates
;; (this could actually be done out of order)
(lp/defpred 'even '(-> i o))
(lp/defpred 'odd '(-> i o))

;; We need two rules for even
(lp/addclause '((even O)))
(lp/addclause '((even (S N)) :- (odd N)))

;; And only one for odd, using even
(lp/addclause '((odd (S N)) :- (even N)))

(lp/type-check-program?)
