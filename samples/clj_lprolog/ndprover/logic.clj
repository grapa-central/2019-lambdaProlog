(ns clj-lprolog.ndprover.logic
  (:require [clj-lprolog.core :as lp]))

(lp/deftype 'j)
(lp/deftype 'bool)

(lp/defconst 'and '(-> bool bool bool))
(lp/defconst 'or '(-> bool bool bool))
(lp/defconst 'imp '(-> bool bool bool))
(lp/defconst 'neg '(-> bool bool))
(lp/defconst 'forall '(-> (-> j bool) bool))
(lp/defconst 'some '(-> (-> j bool) bool))
(lp/defconst 'perp 'bool)
