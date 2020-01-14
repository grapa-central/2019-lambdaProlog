(ns clj-lprolog.solve
  (:require [clj-lprolog.utils :as u :refer [example examples ok>]]
            [clj-lprolog.unif :as uni]
            [clj-lprolog.syntax :as syn]
            [clj-lprolog.typecheck :as typ]))

(def +examples-enabled+ true)

;;{
;; # Renaming utilities
;;
;; In order to avoid naming conflict, we will systematically rename free variables
;; on a clause-by-clause basis. The renaming consists in suffixing the variable
;; name with a number.
;; Of course, typing information is preserved throughout
;;}

(defn instantiate-free-var
  "Actually instantiate `var` by suffixing it with `count`"
  [count var] (symbol (str var "_" count)))

(defn instantiate-term
  "Instantiate the term `t` by suffixing free-variables with `count`"
  [count t]
  (with-meta
    (cond
      (syn/free? t) (instantiate-free-var count t)
      (syn/lambda? t) (list 'λ (second t) (instantiate-term count (nth t 2)))
      (syn/application? t) (map (fn [t] (instantiate-term count t)) t)
      :else t)
    {:ty (typ/type-of t)}))

(defn instantiate-pred
  "Instantiate the predicate `p` by suffixing free-variables with `count`"
  [count p] (map (fn [t] (instantiate-term count t)) p))

(defn instantiate-clause
  "Instantiate the clause `cl` by suffixing free-variables with `count`"
  [count cl] [(instantiate-pred count (first cl))
              (map (fn [p] (instantiate-pred count p)) (second cl))])

(example
 (instantiate-clause 42 '[(even (succ N)) ((odd N))])
 => '[(even (succ N_42)) ((odd N_42))])

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
       [:ok (set (map
                  (fn [subst]
                    [subst
                     (map (fn [p] (map (fn [t] (uni/apply-subst subst t)) p))
                          body)]) substs))])))

(example
 (unify-clause '(even (succ zero)) '[(even (succ N)) ((odd N))])
 => '[:ok #{[{N zero} ((odd zero))]}])

(unify-clause '(append (cs zero ni) L L)
              '[(append (cs X L1) L2 (cs X L3)) ((append L1 L2 L3))])

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

(defn solve
  "Solve `req` in the context of the program `prog`"
  ([prog req]
   (ok> (solve prog req {} 0) :as [_ [subst _]]
        (uni/get-free-vars req) :as freevars
        [:ok (u/map-of-pair-list
              (map (fn [x] [x (uni/apply-subst subst x)]) freevars))]))
  ([prog req si cnt]
   (ok>
    ;; Find the applicable clause bodies
    (compatible-clauses (second (get prog (first req))) req si cnt) :as [poss cnt]
    ;; If we didn't find any result, ko
    (when (empty? poss) [:ko 'solve {:req req}])
    ;; Did we find any immediate result ?
    (if-let [res (some (fn [p] (when (empty? (second p)) p)) poss)]
      [:ok [(first res) cnt]]
      (ok>
       (map
        (fn [[si cl]]
          (ok> (u/ok-reduce
                (fn [[si cnt] p]
                  (solve prog (uni/apply-subst si p) si cnt)) [si cnt] cl)))
        poss) :as ress
       (filter u/ok-expr? ress) :as ress
       (when (empty? ress) [:ko 'solve {:req req}])
       (first ress)))
    )))

(example
 (solve '{even [(-> nat o) {(even zero) (), (even (succ (succ N))) ((even N))}]}
       '(even (succ (succ (succ N))))) => '[:ok {N (succ zero)}])
