(ns clj-lprolog.presyntax
  "User lambda-calculus syntax"
  (:require [clj-lprolog.utils :as u :refer [example examples ok>]]
            [clojure.string :as str]
            [clj-lprolog.syntax :as syn]))

(def +examples-enabled+ true)

;;{
;; # Kernel terms
;;
;; A kernel lambda-term is either:
;; - a bound variable (identified by its De Bruijn index)
;; - a free (substituable) variable (identified by a symbol)
;; - a constant (O, S, +, *, or user-declared)
;; - a n-ary λ-abstraction
;; - a n-ary application
;;}

(declare proper-kernel-term?)

(defn proper-lambda?
  "Is `t` a well-formed λ-abstraction ?"
  [t] (and (syn/lambda? t)
           (proper-kernel-term? (nth t 2))))

(defn proper-application?
  "Is `t` a well-formed application ?"
  [t] (and (syn/application? t)
           (every? proper-kernel-term? t)))

(defn proper-kernel-term?
  "Is `t` a kernel term ?"
  [t] (or (syn/bound? t)
          (syn/free? t)
          (syn/string-lit? t)
          (syn/int-lit? t)
          (syn/boolean-lit? t)
          (syn/primitive? t)
          (syn/user-const? t)
          (proper-lambda? t)
          (proper-application? t)))

(example (proper-application? '((λ 2 #{0}) A B)) => true)
(example (proper-lambda? '(λ 2 #{0})) => true)

;;{
;; # Type syntax
;;
;; A type is either
;; - a type variable (identified by an capitalized symbol)
;; - the primitive "nat" type `i`
;; - the primitive "prop" type `o`
;; - a n-ary arrow type
;;}

(declare proper-type?)

(defn proper-arrow-type?
  "Is `t` an arrow type ?"
  [t] (and (syn/arrow-type? t)
           (every? proper-type? (rest t))))

(defn proper-applied-type-constructor?
  "Is `t` an applied type constructor ?"
  [t] (and (syn/applied-type-constructor? t) (every? proper-type? (rest t))))

(defn proper-type?
  "Is `t` a proper type ?"
  [t] (or (syn/type-var? t)
          (syn/prop-type? t)
          (syn/string-type? t)
          (syn/int-type? t)
          (syn/boolean-type? t)
          (syn/user-type? t)
          (proper-arrow-type? t)
          (proper-applied-type-constructor? t)))

(example (proper-arrow-type? '(-> A (-> B C))) => true)
(example (proper-applied-type-constructor? '(pair nat bool)) => true)

;;{
;; # User terms
;;
;; An user lambda-term is either:
;; - a bound variable (identified by a lowercase symbol)
;; - a free variable (identified by an uppercase symbol)
;; - a primitive constant
;; - a n-ary λ-abstraction
;; - a n-ary application
;;}

(defn bound-or-const?
  "Is `t` a bound variable or a constant ?"
  [t] (and (symbol? t)
           (not (some #{t} syn/reserved))
           (= (symbol (str/lower-case t)) t)))

(example (bound-or-const? 'x) => true)

(defn lambda?
  "Is `t` a λ-abstraction ?"
  [t] (and (seq? t) (= (count t) 3)
           (= (first t) 'λ)
           (vector? (second t)) (every? bound-or-const? (second t))))

(example (lambda? '(λ [x y] (+ x y))) => true)

(defn user-term?
  "Is `t` a user term ?"
  [t] (or (bound-or-const? t)
          (syn/free? t)
          (syn/string-lit? t)
          (syn/primitive? t)
          (lambda? t)
          (syn/application? t)))

(defn parse
  "Parse a user term `t` to a kernel term.
  Mainly transforms bound variables to De Bruijn indices"
  ([t] (ok> (parse t '{}) :as [_ t']
            [:ko> 'parsing-term {:t t}]
            (if (proper-kernel-term? t') [:ok t']
                [:ko 'not-a-proper-kernel-term {:t t'}])))
  ([t env]
  (cond (bound-or-const? t)
        (if (contains? env t)
          ;; t is really a bound variable
          [:ok #{(get env t)}]
          ;; t is actually a use constant
          [:ok t])

        (syn/free? t) [:ok t]
        (syn/string-lit? t) [:ok t]
        (syn/int-lit? t) [:ok t]
        (syn/boolean-lit? t) [:ok t]
        (syn/primitive? t) [:ok t]

        (lambda? t)
        (ok>
         (count (second t)) :as n
         (u/map-of-pair-list
          (map (fn [[x ind]] [x (+ ind n)]) env)) :as env
         (reduce (fn [[e n] x] [(assoc e x n) (dec n)])
                 [env (dec n)] (second t)) :as [env _]
         (parse (nth t 2) env) :as [_ t']
         [:ok (list 'λ n t')])

        (syn/application? t)
        (ok> (u/ok-map (fn [t] (parse t env)) t) :as [_ t']
             [:ok (map (fn [[t]] t) t')])

        :else [:ko 'not-a-user-term {:t t}])))

;;{
;; # Prolog vernacular
;;
;; The user can define predicates using the macro `defpred`
;; The parameters of the macro are:
;; - the name of the predicate
;; - its type
;; They can then populate the predicate with the macro `addclause`
;; which takes as argument a horn clause, with the conclusion
;; (which must be the predicate applied to the right number of arguments)
;; as its head, and some other predicates applied as its body
;;}

(defn parse-applied-pred
  "Parse the arguments of an applied predicate `p`"
  [p] (ok> (u/ok-map parse (rest p)) :as [_ tl]
           [:ko> 'parsing-applied-pred {:pred p}]
           [:ok (cons (first p) (map (fn [[t]] t) tl))]))

(defn proper-typed-binding?
  "Check that `t` is a proper typed-binding"
  [t] (and (seq? t) (= 3 (count t)) (= :> (second t))
           (syn/user-const? (first t)) (proper-type? (nth t 2))))

(example (proper-typed-binding? '(x :> i)) => true)

(declare parse-clause-body)

(defn parse-goal
  "Parse one of the goals `g` in a clause"
  [g] (cond
        ;; The body is a pi-abstraction
        (syn/pi? g)
        (ok>
         (when (not (proper-typed-binding? (second g))))
         [:ko 'parse-goal {:goal g}]
         (parse-clause-body (nthrest g 2)) :as [_ body]
         [:ok (list 'Π (second g) body)])
        ;; The body is an implication
        (syn/imp? g)
        (ok>
         (parse-applied-pred (second g)) :as [_ hd]
         (parse-clause-body (nthrest g 2)) :as [_ body]
         [:ok (list '=> hd body)])
        ;; The first term of the body is an applied predicate
        (syn/applied-pred? g) (parse-applied-pred g)
        :else [:ko 'parse-goal {:goal g}]))

(defn parse-clause-body
  "Parse the body `b` of a clause"
  [b] (cond
        ;; A clause body cannot be empty
        (or (not (seq? b)) (empty? b)) [:ko 'parse-clause-body {:body b}]
        ;; If this is the last one
        (empty? (rest b))
        (ok> (parse-goal (first b)) :as [_ hd]
             [:ok (list hd)])
        :else
        (ok> (parse-goal (first b)) :as [_ hd]
             (parse-clause-body (rest b)) :as [_ tl]
             [:ok (cons hd tl)])))

(defn parse-clause
  "Parse the clause `c`"
  [c] (ok> (when (not (syn/clause? c)) [:ko 'parse-clause {:clause c}])
           (parse-applied-pred (first c)) :as [_ hd]
           [:ko> 'parse-clause {:clause c}]
           (if (empty? (rest c)) [:ok '()]
               (parse-clause-body (nthrest c 2))) :as [_ tl]
           [:ko> 'parse-clause {:clause c}]
           [:ok [hd tl]]))

(defn user-type-dec?
  "Is `ty` a well-formed type definition"
  [ty] (or (syn/user-type? ty)
           (and (syn/applied-type-constructor? ty)
                (every? syn/type-var? (rest ty)))))

(examples
 (user-type-dec? 'nat) => true
 (user-type-dec? '(list A)) => true)

(defn user-const-dec?
  "Is `c` a well formed constant declaration with type `ty`"
  [c ty] (and (syn/user-const? c) (proper-type? ty)))

(example (user-const-dec? 'ni '(list A)) => true)
