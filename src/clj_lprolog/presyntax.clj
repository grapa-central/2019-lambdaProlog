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
          (syn/nat-type? t)
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
           (not= (symbol (str/capitalize t)) t)))

(example (bound-or-const? 'x) => true)

(defn free?
  "Is `t` a free variable ?"
  [t] (and (symbol? t)
           (not (some #{t} syn/reserved))
           (= (symbol (str/capitalize t)) t)))

(example (free? 'X) => true)

(defn lambda?
  "Is `t` a λ-abstraction ?"
  [t] (and (seq? t) (= (count t) 3)
           (= (first t) 'λ)
           (vector? (second t)) (every? bound-or-const? (second t))))

(example (lambda? '(λ [x y] (+ x y))) => true)

(defn user-term?
  "Is `t` a user term ?"
  [t] (or (bound-or-const? t)
          (free? t)
          (syn/primitive? t)
          (lambda? t)
          (syn/application? t)))

(defn parse-aux
  "Parse a user term `t` to a kernel term using a naming environment"
  [t env]
  (cond (bound-or-const? t)
        (if (contains? (first env) t)
          ;; t is really a bound variable
          [:ok #{(get (first env) t)}]
          ;; t is actually a use constant
          [:ok t])

        (free? t) [:ok t]

        (syn/primitive? t) [:ok t]

        (lambda? t)
        (ok> (parse-aux
              (nth t 2)
              (reduce (fn [e x]
                        (list
                         (assoc (first e) x (second e))
                         (+ (second e) 1)))
                      env (second t))) :as [_ t']
        [:ok (list 'λ (count (second t)) t')])

        (syn/application? t)
        (ok> (u/ok-map (fn [t] (parse-aux t env)) t) :as [_ t']
             [:ok (map (fn [[t]] t) t')])

        :else [:ko 'not-a-user-term {:t t}]))

(defn parse
  "Parse a user term `t` to a kernel term.
  Mainly transforms bound variables to De Bruijn indices"
  [t] (ok> (parse-aux t '({} 0)) :as [_ t']
           [:ko> 'parsing-term {:t t}]
           (if (proper-kernel-term? t') [:ok t']
               [:ko 'not-a-proper-kernel-term {:t t'}])))


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

(defn pred?
  "Is `t` a predicate (either defined or free variable) ?"
  [t] (symbol? t))

(example (pred? 'even) => true)

(defn applied-pred?
  "Is `t` an applied predicate ?"
  [t] (and (seq? t) (pred? (first t))))

(example (applied-pred? '(even (S N))) => true)

(defn parse-applied-pred
  "Parse the arguments of an applied predicate `p`"
  [p] (ok> (u/ok-map parse (rest p)) :as [_ tl]
           [:ko> 'parsing-applied-pred {:pred p}]
           [:ok (cons (first p) (map (fn [[t]] t) tl))]))

(defn clause-body?
  "Is `t` a clause body ?"
  [t] (every? applied-pred? t))

(example (clause-body? '((even N) (even O))) => true)

(defn clause?
  "Is `t` a clause ?"
  [t] (and (seq? t)
           (applied-pred? (first t))
           (or (clause-body? (rest t))
               (and (= (second t) ':-) (clause-body? (nthrest t 2))))))

(examples
 (clause? '((even (S N)) :- (even N))) => true
 (clause? '((even (S N)) (even N))) => true)

(defn parse-clause
  "Parse the clause `c` (removes the :-)"
  [c] (ok> (when (not (clause? c)) [:ko 'parse-clause {:clause c}])
           (parse-applied-pred (first c)) :as [_ hd]
           [:ko> 'parsing-clause {:clause c}]
           (if (clause-body? (rest c))
             (u/ok-map parse-applied-pred (rest c))
             (u/ok-map parse-applied-pred (nthrest c 2))) :as [_ tl]
           [:ko> 'parsing-clause {:clause c}]
           [:ok (cons hd (map (fn [[t]] t) tl))]))

;; Contains the set of user types during execution of the program
(def progtypes (atom #{}))

(defn user-type-dec?
  "Is `ty` a well-formed type definition"
  [ty] (or (syn/user-type? ty)
           (and (syn/applied-type-constructor? ty) (every? syn/type-var? (rest ty)))))

(examples
 (user-type-dec? 'nat) => true
 (user-type-dec? '(list A)) => true)

(defmacro deftype
  "Define a type. `n` is the name of the type, it should be lowercase"
  [ty] `(if (user-type-dec? ~ty)
          (swap! progtypes (fn [pt#] (conj pt# ~ty)))
          [:ko 'deftype {:n ~ty}]))

;; Contains the set of constants (and their types) during execution of the program
(def progconsts (atom {}))

(defmacro defconst
  "Define a constant `n` (should be lowercase),
  with its type `ty`. Checks well formedness"
  [n ty] `(if (and (symbol? ~n) (= (symbol (str/lower-case ~n)) ~n)
                   (proper-type? ~ty))
            (swap! progconsts (fn [pc#] (assoc pc# ~n ~ty)))
            [:ko 'defconst {:n ~n :ty ~ty}]))

;; Contains the set of predicates during execution of the program
(def progpreds (atom {}))

(defmacro defpred
  "Define a predicate. `n` is the name of the predicate, and `t` its type
  Also check well-formedness"
  [n t]
  `(if (and (pred? ~n) (proper-type? ~t))
     (swap! progpreds (fn [pp#] (assoc pp# ~n [~t {}])))
  [:ko 'defpred {:n ~n :t ~t}]))

(defmacro addclause
  "Add `clause` to a predicate.
   Also checks that the clause is well formed"
  [clause]
  `(let [clause# (parse-clause ~clause)]
     (if (u/ko-expr? clause#) [:ko 'addclause {:cause clause#}]
         (let [head# (first (second clause#))
               body# (rest (second clause#))]
           (swap! progpreds
                  (fn [pp#]
                    (if-let [prev# (get @progpreds (first head#))]
                      (assoc pp# (first head#)
                             (list (first prev#)
                                   (assoc (second prev#) head# body#))))))))))

(defn start
  "Reset the program environment"
  [] (do (swap! progtypes (fn [_] #{}))
         (swap! progconsts (fn [_] {}))
         (swap! progpreds (fn [_] {}))))
