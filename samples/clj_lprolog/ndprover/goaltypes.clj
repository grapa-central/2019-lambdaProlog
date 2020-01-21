(ns clj-lprolog.ndprover.goaltypes
  (:require [clj-lprolog.core :as lp]))

(lp/deftype 'goal)

(lp/defconst 'truegoal 'goal)
(lp/defconst 'andgoal '(-> goal goal goal))
(lp/defconst 'allgoal '(-> (-> A goal) goal))

(lp/type-check-program)
