(ns clj-lprolog.ndprover.ndproofs
  (:require [clj-lprolog.core :as lp]))

(load-file "samples/clj_lprolog/ndprover/logic.clj")

(lp/deftype 'proof-object)

(lp/defconst 'and-i '(-> proof-object proof-object proof-object))
(lp/defconst 'or-i1 '(-> proof-object proof-object))
(lp/defconst 'or-i2 '(-> proof-object proof-object))
(lp/defconst 'imp-i '(-> (-> proof-object proof-object) proof-object))
(lp/defconst 'forall-i '(-> (-> j proof-object) proof-object))
(lp/defconst 'exists-i '(-> j proof-object proof-object))
(lp/defconst 'and-e1 '(-> proof-object proof-object))
(lp/defconst 'and-e2 '(-> proof-object proof-object))
(lp/defconst 'imp-e '(-> j proof-object proof-object))
(lp/defconst 'forall-e '(-> j proof-object proof-object))
(lp/defconst 'or-e '(-> proof-object (-> proof-object proof-object) (-> proof-object proof-object) proof-object))
(lp/defconst 'exists-e '(-> proof-object (-> j proof-object proof-object) proof-object))

(lp/type-check-program)

