(ns clj-lprolog.ndprover.nonlogical
  (:require [clj-lprolog.core :as lp]))

(load-file "samples/clj_lprolog/ndprover/logic.clj")

(lp/defconst 'a 'j)
(lp/defconst 'b 'j)
(lp/defconst 'jar 'j)

(lp/defconst 'p '(-> j bool))
(lp/defconst 'q '(-> j bool))
(lp/defconst 'sterile '(-> j bool))
(lp/defconst 'dead '(-> j bool))
(lp/defconst 'animal '(-> j bool))
(lp/defconst 'bug '(-> j bool))
(lp/defconst 'heated '(-> j bool))

(lp/defconst 'in '(-> j j bool))

(lp/type-check-program)
