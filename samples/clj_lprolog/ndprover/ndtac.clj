(ns clj-lprolog.ndprover.ndtac
  (:require [clj-lprolog.core :as lp]))

(load-file "samples/clj_lprolog/nat.clj")
(load-file "samples/clj_lprolog/ndprover/goaltypes.clj")
(load-file "samples/clj_lprolog/ndprover/logic.clj")
(load-file "samples/clj_lprolog/ndprover/ndproofs.clj")
(load-file "samples/clj_lprolog/ndprover/listmanip.clj")

(lp/deftype 'judgment)
(lp/deftype 'answer)

(lp/defconst 'of-type '(-> proof-object bool judgment))
(lp/defconst '--> '(-> (list judgment) judgment goal))

(lp/defconst 'yes 'answer)


(lp/defpred 'exists-e-tac '(-> nat goal goal o))
(lp/defpred 'or-e-tac '(-> nat goal goal o))
(lp/defpred 'forall-e-query '(-> nat goal goal o))
(lp/defpred 'forall-e-tac '(-> nat goal goal o))
(lp/defpred 'fchain-tac '(-> nat goal goal o))
(lp/defpred 'bchain-tac '(-> nat goal goal o))
(lp/defpred 'imp-e-retain '(-> nat goal goal o))
(lp/defpred 'impt-e-tac '(-> nat goal goal o))
(lp/defpred 'and-e-tac '(-> nat goal goal o))
(lp/defpred 'exists-i-query '(-> goal goal o))
(lp/defpred 'exists-i-tac '(-> goal goal o))
(lp/defpred 'forall-i-tac '(-> goal goal o))
(lp/defpred 'imp-i-tac '(-> goal goal o))
(lp/defpred 'or-i2-tac '(-> goal goal o))
(lp/defpred 'or-i1-tac '(-> goal goal o))
(lp/defpred 'and-i-tac '(-> goal goal o))
(lp/defpred 'close-tacn '(-> nat goal goal o))
(lp/defpred 'close-tac '(-> goal goal o))


(lp/addclause '((close-tac (--> Gamma (of-type P A)) truegoal) :- (member (of-type P A) Gamma)))

(lp/addclause '((close-tacn N (--> Gamma (of-type P A)) truegoal) :- (nth-item N (of-type P A) Gamma)))

(lp/addclause '((and-i-tac (--> Gamma (of-type (and-i P1 P2) (and A B)))
                           (andgoal (--> Gamma (of-type P1 A)) (--> Gamma (of-type P2 B))))))

+(lp/addclause '((or-i1-tac (--> Gamma (of-type (or-i1 P) (or A B)))
+                           (--> Gamma (of-type P A)))))
+
+(lp/addclause '((or-i2-tac (--> Gamma (of-type (or-i2 P) (or A B)))
+                           (--> Gamma (of-type P B)))))
+
+(lp/addclause '((imp-i-tac (--> Gamma (of-type (imp-i P) (imp A B)))
+                           (allgoal (λ [pa] (--> (cs (of-type pa A) Gamma) (of-type (P pa) B)))))))
+
+;; (lp/addclause '((forall-i-tac (--> Gamma (of-type (forall-i P) (forall))))))
