(ns clj-lprolog.ndprover.listmanip
  (:require [clj-lprolog.core :as lp]))

(load-file "samples/clj_lprolog/nat.clj")
(load-file "samples/clj_lprolog/lists.clj")

(lp/defpred 'member-and-rest '(-> A (list A) (list A) o))
(lp/defpred 'nth-item '(-> nat A (list A) o))
(lp/defpred 'nth-item-and-rest '(-> nat A (list A) (list A) o))
(lp/defpred 'member-move-to-end '(-> A (list A) (list A) o))
(lp/defpred 'add-to-end '(-> A (list A) (list A) o))

(lp/addclause '((member-and-rest A (cs A L) L)))

(lp/addclause '((member-and-rest A (cs B L1) (cs B L2)) :-
                (member-and-rest A L1 L2)))

(lp/addclause '((nth-item zero A L) :- (member A L)))

(lp/addclause '((nth-item (succ zero) A (cs A R))))

(lp/addclause '((nth-item (succ N) A (cs B L)) :- (nth-item N A L)))

(lp/addclause '((nth-item-and-rest zero A Lst Rest) :-
                (member-and-rest A Lst Rest)))

(lp/addclause '((nth-item-and-rest (succ zero) A (cs A Rest) Rest)))
(lp/addclause '((nth-item-and-rest (succ N) A (cs B Tail) (cs B Rest)) :-
                (nth-item-and-rest N A Tail Rest)))

(lp/addclause '((member-move-to-end A (cs A Rest) Newlist) :-
                (add-to-end A Rest Newlist)))
(lp/addclause '((member-move-to-end A (cs B Tail) (cs B Newlist)) :-
                (add-to-end A Rest Newlist)))

(lp/addclause '((add-to-end A ni (cs A ni))))
(lp/addclause '((add-to-end A (cs Head Tail) (cs Head Newtail)) :-
                (add-to-end A Tail Newtail)))



