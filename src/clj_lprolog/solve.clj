(ns clj-lprolog.solve
  (:require [clj-lprolog.utils :as u :refer [example examples ok>]]
            [clj-lprolog.unif :as uni]
            [clj-lprolog.syntax :as syn]
            [clj-lprolog.typecheck :as typ]
            [clojure.string :as str]))

(def +examples-enabled+ true)

;;{
;; # Renaming utilities
;;
;; In order to avoid naming conflict, we will systematically rename free variables
;; on a clause-by-clause basis. The renaming consists in suffixing the variable
;; name with a number.
;; We must also rename variables bound by a pi-abstraction
;; Of course, typing information is preserved throughout
;;}

(defn instantiate-free-var
  "Actually instantiate `var` by suffixing it with `count`"
  [count var] (if (str/includes? (str var ) "_") var ;; Do not instantiate twice !
                  (symbol (str var "_" count))))

(defn instantiate-term
  "Instantiate the term `t` by suffixing free-variables with `count`"
  [count t]
  (typ/set-type
    (cond
      (syn/free? t) (instantiate-free-var count t)
      (syn/lambda? t) (list 'λ (second t) (instantiate-term count (nth t 2)))
      (syn/application? t) (map (fn [t] (instantiate-term count t)) t)
      :else t)
    (typ/type-of t)))

(defn instantiate-pred
  "Instantiate the predicate `p` by suffixing free-variables with `count`"
  [count p] (map (fn [t] (instantiate-term count t)) p))

(declare instantiate-clause-body)

(defn instantiate-goal
  "Instantiate a goal `g` by suffixing free-variables with `count`"
  [count g]
  (cond
    (syn/pi? g) (list 'Π (second g) (instantiate-clause-body count (nth g 2)))
    (syn/imp? g) (list '=> (instantiate-pred count (second g))
                       (instantiate-clause-body count (nth g 2)))
    (syn/applied-pred? g) (instantiate-pred count g)))

(defn instantiate-clause-body
  "Instantiate the clause body `b` by suffixing free-variables with `count`"
  [count b] (if (empty? b) '()
                (cons (instantiate-goal count (first b))
                      (instantiate-clause-body count (rest b)))))

(defn instantiate-clause
  "Instantiate the clause `cl` by suffixing free-variables with `count`"
  [count cl] [(instantiate-pred count (first cl))
              (instantiate-clause-body count (second cl))])

(example
 (instantiate-clause 42 '[(even (succ N)) ((odd N))])
 => '[(even (succ N_42)) ((odd N_42))])

(defn instantiatepi-const
  "Actually instantiate `var` by suffixing it with `count`"
  [count var] (symbol (str var "_" count)))

(defn instantiatepi-term
  "Instantiate the term `t` by suffixing occurences of `x` with `count`"
  [x count t]
  (typ/set-type
    (cond
      (and (syn/user-const? t) (= t x)) (instantiatepi-const count x)
      (syn/lambda? t) (list 'λ (second t) (instantiatepi-term x count (nth t 2)))
      (syn/application? t) (map (fn [t] (instantiatepi-term x count t)) t)
      :else t)
    (typ/type-of t)))

(defn instantiatepi-pred
  "Instantiate the predicate `p` by suffixing occurences of `x` with `count`"
  [x count p] (map (fn [t] (instantiatepi-term x count t)) p))

(declare instantiatepi-clause-body)

(defn instantiatepi-goal
  "Instantiate a goal `g` by suffixing occurences of `x` with `count`"
  [x count g]
  (cond
    (syn/pi? g) (list 'Π (second g) (instantiatepi-clause-body x count (nth g 2)))
    (syn/imp? g) (list '=> (instantiatepi-pred x count (second g))
                       (instantiatepi-clause-body x count (nth g 2)))
    (syn/applied-pred? g) (instantiatepi-pred x count g)))

(defn instantiatepi-clause-body
  "Instantiate the clause body `b` by suffixing occurences of `x` with `count`"
  [x count b] (if (empty? b) '()
                (cons (instantiatepi-goal x count (first b))
                      (instantiatepi-clause-body x count (rest b)))))

(defn instantiatepi-clause
  "Instantiate the clause `cl` by suffixing occurences of `x` with `count`"
  [x count cl] [(instantiatepi-pred x count (first cl))
              (instantiatepi-clause-body x count (second cl))])

(example
 (instantiatepi-clause 'x 42 '[(even (succ x)) ((odd N))])
 => '[(even (succ x_42)) ((odd N))])

;;{
;; # Solving algorithm
;;
;; Similar to Prolog
;;}

(defn unify-clause
  "Unify the parameters of `req` with those of the head of the clause `cl`.
  Return a set of substitutions, and bodys of `cl` with the substitution applied"
  ([req cl] (unify-clause req cl {}))
  ([req [head body] si]
  (ok> (uni/unify req head) :as [_ substs]
       (map (fn [si'] (uni/compose-subst si si')) substs) :as substs
       (map (fn [[_ si]] si) (filter u/ok-expr? substs)) :as substs
       (uni/occur-check-substs substs) :as substs
       [:ok (set (map (fn [subst] [subst body]) substs))])))

(example
 (unify-clause '(even (succ zero)) '[(even (succ N)) ((odd N))])
 => '[:ok #{[{N zero} ((odd N))]}])

(defn compatible-clauses
  "Get the instantiated clauses compatibles with `req` among `clauses`.
  Also manipulates the fresh-variable counter `cnt`"
  [clauses req si cnt]
  (ok>
   ;; Instantiate fresh variable names
   (reduce
    (fn [[clauses cnt] cl] [(cons (instantiate-clause cnt cl) clauses) (inc cnt)])
    ['() cnt] clauses) :as [clauses cnt]
   (map (fn [cl] (unify-clause req cl si)) clauses) :as poss
   (map (fn [[_ x]] x) (filter u/ok-expr? poss)) :as poss
   [(reduce (fn [s1 s2] (clojure.set/union s1 s2)) #{} poss)
    (+ cnt (count clauses))]))

(declare solve)

(defn solve-body
  "Solve the clause body `b` in the context of the program `prog`"
  [prog [si req] cnt]
  (cond (empty? req) [:ok si]
        ;; Pi-abstraction : instantiate the constants with a fresh identifier
        (syn/pi? (first req))
        (ok> (first (second (first req))) :as x
             (instantiatepi-clause-body x cnt (nth (first req) 2)) :as req1
             (rest req) :as req2
             (solve-body prog [si (concat req1 req2)] (inc cnt)))
        ;; Implication : add a dynamic clause to the program
        (syn/imp? (first req))
        (ok> (uni/apply-subst si (second (first req))) :as assump
             (nth (first req) 2) :as req1
             (rest req) :as req2
             (get prog (first assump)) :as [ty clauses]
             (cons [assump '()] clauses) :as clauses
             (assoc prog (first assump) [ty clauses]) :as prog
             (solve-body prog [si (concat req1 req2)] (inc cnt)))
        ;; Solve an applied predicate
        (syn/applied-pred? (first req)) (solve prog [si req] cnt)))

(defn solve
  "Solve `req` in the context of the program `prog`"
  ([prog req]
   (ok> (solve-body prog [{} (list req)] 0) :as [_ subst]
        (uni/get-freevars req) :as freevars
        [:ok (u/map-of-pair-list
              (map (fn [x] [x (uni/apply-subst subst x)]) freevars))]))
  ([prog [si req] cnt]
   (ok>
    ;; Let's try to solve the first constraint
    (uni/apply-subst si (first req)) :as scrut
    ;; Find the applicable clause bodies
    (compatible-clauses (second (get prog (first scrut)))
                        scrut si cnt) :as [poss cnt]
    ;; If we didn't find any result, ko
    (when (empty? poss) [:ko 'solve {:req req}])
    ;; Research heuristic : start with the possibilities
    ;; that add the least number of clauses
    (sort (fn [[_ b1] [_ b2]] (compare (count b1) (count b2))) poss) :as poss
    ;; Recursive calls (return the first correct one)
    (some
     (fn [[si cl]]
       (let [res (solve-body prog [si (concat cl (rest req))] cnt)]
         (when (u/ok-expr? res) (second res))))
     poss) :as ress
    (when (nil? ress) [:ko 'solve {:req req}])
    [:ok ress])))

(example
 (solve '{even [(-> nat o) {(even zero) (), (even (succ (succ N))) ((even N))}]}
       '(even (succ (succ (succ N))))) => '[:ok {N (succ zero)}])
