(ns clj-lprolog.ndprover.tacticals
  (:require [clj-lprolog.core :as lp]))

(load-file "samples/clj_lprolog/ndprover/goaltypes.clj")
(load-file "samples/clj_lprolog/ndprover/goalred.clj")

(lp/defpred 'maptac '(-> (-> goal goal o) goal goal o))
(lp/defpred 'then '(-> (-> goal goal o) (-> goal goal o) goal goal o))
(lp/defpred 'orelse '(-> (-> goal goal o) (-> goal goal o) goal goal o))
(lp/defpred 'idtac '(-> goal goal o))
(lp/defpred 'repeattac '(-> (-> goal goal o) goal goal o))
(lp/defpred 'try '(-> (-> goal goal o) goal goal o))
(lp/defpred 'complete '(-> (-> goal goal o) goal goal o))

(lp/addclause '((maptac Tac truegoal truegoal)))
(lp/addclause '((maptac Tac (andgoal Ingoal1 Ingoal2) Outgoal) :-
                (maptac Tac Ingoal1, Outgoal1),
                (maptac Tac Ingoal2, Outgoal2),
                (goalreduce (andgoal Outgoal1 Outgoal2) Outgoal)))

;;add clause with pi

(lp/addclause '((maptac Tac Ingoal Outgoal) :-
                (Tac Ingoal Outgoal)))

(lp/addclause '((then Tac1 Tac2 Ingoal Outgoal) :-
                (Tac1 Ingoal Midgoal),
                (maptac Tac2 Midgoal Outgoal)))

(lp/addclause '((orelse Tac1 Tac2 Ingoal Outgoal) :-
               (Tac1 Ingoal Outgoal),
               (Tac2 Ingoal Outgoal)))

(lp/addclause '((idtac Goal Goal)))

(lp/addclause '((repeattac Tac Ingoal Outgoal) :-
                (orelse (then Tac (repeattac Tac)) idtac Ingoal Outgoal)))

(lp/addclause '((try Tac Ingoal Outgoal) :-
                (orelse Tac idtac Ingoal Outgoal)))

(lp/addclause '((complete Tac Ingoal truegoal) :-
                (Tac Ingoal truegoal)))

(lp/type-check-program)
