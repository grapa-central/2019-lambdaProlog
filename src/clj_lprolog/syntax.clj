(ns clj-lprolog.syntax
  "Kernel syntax"
  (:require [clojure.string :as str]))

;;{
;; # Kernel terms
;;
;; A kernel lambda-term is either:
;; - a bound variable (identified by its De Bruijn index)
;; - a free (substituable) variable (identified by a symbol)
;; - a primitive constant (∀, =>, O, S, +, *)
;; - a n-ary λ-abstraction
;; - a n-ary application
;;}

(declare kernel-term?)

(def reserved
  "Reserved symbols"
  '(λ ∀ => O S + *))

(defn bound?
  "Is `t` a bound variable ?"
  [t] (nat-int? t))

(defn free?
  "Is `t` a free variable ?"
  [t] (and (symbol? t) (not (some #{t} reserved))))

(defn primitive?
  "Is `t` a primitive constant ?"
  [t] (some #{t} (nthrest reserved 2)))

(defn lambda?
  "Is `t` a λ-abstraction ?"
  [t] (and (seq? t)
           (= (first t) 'λ)
           (nat-int? (second t))
           (kernel-term? (nth t 2))))

(defn application?
  "Is `t` an application ?"
  [t] (and (seq? t)
           (not (empty? t)) (not (empty? (rest t)))
           (every? kernel-term? t)))

(defn kernel-term?
  "Is `t` a kernel term ?"
  [t] (or (bound? t)
          (free? t)
          (primitive? t)
          (lambda? t)
          (application? t)))

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

(defn type-var?
  "Is `t` a type variable ?"
  [t] (and (symbol? t)
           (= (symbol (str/capitalize t)) t)))

(defn nat-type?
  "Is `t` the nat type ?"
  [t] (= t 'i))

(defn prop-type?
  "Is `t` the proposition type ?"
  [t] (= t 'o))

(defn arrow-type?
  "Is `t` an arrow type ?"
  [t] (and (seq? t)
           (> (count t) 2)
           (= (first t) '->)
           (every? proper-type? (rest t))))

(defn proper-type?
  "Is `t` a proper type ?"
  [t] (or (type-var? t)
          (nat-type? t)
          (prop-type? t)
          (arrow-type? t)))

;;{
;; # Prolog vernacular
;;
;; The user can define predicates using the macro `defpred`
;; The parameters of the macro are:
;; - the name of the predicate
;; - its type
;; - a set of horn clauses, with the conclusion
;;   (which must be the predicate applied to the right number of arguments)
;;   as its head, and some other predicates applied as its body
;;}

(defn pred?
  "Is `t` a predicate ?"
  [t] (and (symbol? t) (not= (symbol (str/capitalize t)) t)))

(defn applied-pred?
  "Is `t` an applied predicate ?"
  [t] (and (seq? t) (pred? (first t))
           (every? kernel-term? (rest t))))

(defn clause-body?
  "Is `t` a clause body ?"
  [t] (or (every? applied-pred? t)))

(defn clause?
  "Is `t` a clause ?"
  [t] (and (seq? t)
           (applied-pred? (first t))
           (or (clause-body? (rest t))
               (and (= (second t) ':-) (clause? (nthrest t 2))))))

(defmacro defpred
  "Define a predicate. `n` is the name of the predicate,
  `t` its type, `body` the list of predicates.
  Also checks that the predicate is well formed. If it's not, return nil"
  [n t & body]
  (if (and (pred? n) (proper-type? t) (every? clause? body))
    `(~n ~t
      ~(map (fn [clause] (filter applied-pred? clause)) body))))
