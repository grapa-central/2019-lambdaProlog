(ns clj-lprolog.ndprover.goalred
  (:require [clj-lprolog.core :as lp]))

(load-file "samples/clj_lprolog/ndprover/goaltypes.clj")

(lp/defpred 'goalreduce '(-> goal goal o))

(lp/addclause '((goalreduce (andgoal truegoal Goal) Outgoal) :- (goalreduce Goal Outgoal)))
(lp/addclause '((goalreduce (andgoal Goal truegoal) OutGoal) :- (goalreduce Goal Outgoal)))

(lp/addclause '((goalreduce (allgoal T) truegoal))) 
(lp/addclause '((goalreduce Goal Goal)))

(lp/type-check-program)
