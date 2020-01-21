(ns clj-lprolog.knowledge
  (:require [clj-lprolog.core :as lp]))

;;{
;; A classic knowledge-base example about family
;;}

(lp/start)

(lp/deftype 'person)
(lp/defconst 'gomez 'person)
(lp/defconst 'morticia 'person)
(lp/defconst 'fester 'person)
(lp/defconst 'grandmama 'person)
(lp/defconst 'wednesday 'person)
(lp/defconst 'pugsley 'person)

(lp/defpred 'mother '(-> person person o))
(lp/addclause '((mother morticia wednesday)))
(lp/addclause '((mother morticia pugsley)))
(lp/addclause '((mother grandmama gomez)))

(lp/defpred 'father '(-> person person o))
(lp/addclause '((father gomez wednesday)))
(lp/addclause '((father gomez pugsley)))

(lp/defpred 'parent '(-> person person o))
(lp/addclause '((parent X Y) :- (father X Y)))
(lp/addclause '((parent X Y) :- (mother X Y)))

(lp/defpred 'parents '(-> person person person o))
(lp/addclause '((parents X Y Z) :- (father X Z), (mother Y Z)))

(lp/defpred 'siblings '(-> person person o))
(lp/addclause '((siblings X Y) :- (parents P M X), (parents P M Y)))

(lp/defpred 'grandparent '(-> person person o))
(lp/addclause '((grandparent X Z) :- (parent X Y), (parent Y Z)))

;; A few requests
(lp/solve '(parent gomez Y)) ;; => [:ok {Y pugsley}]
(lp/solve '(parents X Y wednesday)) ;; => [:ok {X gomez Y morticia}]
(lp/solve '(siblings X pugsley)) ;; => [:ok {X wednesday}]
(lp/solve '(grandparent X Y)) ;; => [:ok {X grandmama Y pugsley}]
