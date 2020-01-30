(ns clj-lprolog.pcf.eval-test
  (:require [clj-lprolog.core :as lp]))

(load-file "samples/clj_lprolog/pcf/eval.clj")
(load-file "samples/clj_lprolog/pcf/examples.clj")

(lp/defpred 'eval-test '(-> int tm o))

(lp/addclause '((eval-test 1 V) :-
                (prog "successor" Suc), (eval (app Suc (in 12)) V)))

(lp/addclause '((eval-test 2 V) :-
                (prog "fib" Fib), (eval (app Fib (in 4)) V)))

(lp/addclause '((eval-test 3 V) :-
                (prog "fact" Fact), (eval (app (app Fact (in 4)) (in 1)) V)))

(lp/addclause '((eval-test 4 V) :- (prog "map" Map) (prog "successor" Succ)
                (eval (app (app Map Succ) (app (app cs (in 1))
                                               (app (app cs (in 41)) ni))) V)))

;;(lp/solve '(eval-test 1 V))
;;(lp/solve '(eval-test 2 V)) ;; This is very long
;;(lp/solve '(eval-test 3 V))
(lp/solve '(eval-test 4 V))
