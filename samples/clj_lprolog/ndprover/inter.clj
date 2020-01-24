(ns clj-lprolog.ndprover.inter
  (:require [clj-lprolog.core :as lp]))

(load-file "samples/clj_lprolog/ndprover/goaltypes.clj")
(load-file "samples/clj_lprolog/ndprover/goalred.clj")
(load-file "samples/clj_lprolog/lists.clj")

(lp/defpred 'inter-top '(-> str proof-object goal o))
(lp/defpred 'inter '(-> goal goal o))
(lp/defpred 'do '(-> o goal goal o))
(lp/defpred 'quitg '(-> goal goal o))
(lp/defpred 'backup '(-> goal goal o))
(lp/defpred 'print-form-list '(-> (list judgement) nat o))
(lp/defpred 'nl 'o)
(lp/defpred 'write '(-> A o))
(lp/defpred 'process-input '((-> goal goal o) goal goal o))

(lp/addclause '((inter-top Name P Outgoal) :-
                (formula Name Formula),
                (inter (--> nil (of-type P Formula Outgoal)))))
(lp/type-check-program)
