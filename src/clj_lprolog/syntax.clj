(ns clj-lprolog.syntax
  "Kernel syntax"
  (:require [clj-lprolog.utils :as u :refer [example examples]]
            [clojure.string :as str]))

(def +examples-enabled+ true)

;;{
;; # Kernel terms
;;
;; A kernel lambda-term is either:
;; - a bound variable (identified by its De Bruijn index)
;; - a free (substituable) variable (identified by a symbol)
;; - a string literal
;; - a constant (O, S, +, *, or user-declared)
;; - a n-ary λ-abstraction
;; - a n-ary application
;; - a suspension (used for explicit substitutions)
;;}

(def reserved
  "Reserved symbols"
  '(λ ∀ => O S + *))

(defn bound?
  "Is `t` a bound variable ?"
  [t] (and (set? t) (nat-int? (first t))))

(example (bound? #{1}) => true)

(defn free?
  "Is `t` a free variable ?"
  [t] (and (symbol? t) (= (symbol (str/capitalize t)) t)
           (not (some #{t} reserved))))

(example (free? 'A) => true)

(defn string-lit?
  "Is `t` a string litteral ?"
  [t] (string? t))

(example (string-lit? "hello") => true)

(defn primitive?
  "Is `t` a primitive constant ?"
  [t] (some? (some #{t} (nthrest reserved 2))))

(examples
 (primitive? 'S) => true
 (primitive? '+) => true)

(defn user-const?
  "Is `t` a user constant ?"
  [t] (and (symbol? t) (= (symbol (str/lower-case t)) t)))

(example (user-const? 'zero) => true)

(defn lambda?
  "Is `t` a λ-abstraction ?"
  [t] (and (seq? t) (= (count t) 3)
           (= (first t) 'λ)
           (nat-int? (second t))))

(example (lambda? '(λ 2 (+ 1 0))) => true)

(defn application?
  "Is `t` an application ?"
  [t] (and (seq? t)
           (not= (first t) 'λ)
           (not (empty? t))))

(example (application? '(S O)) => true)

(defn beta-redex?
  "Is `t` a beta-redex ?"
  [t] (and (application? t) (>= (count t) 2)
           (lambda? (first t))))

(defn suspension?
  "Is `t` a suspension ?"
  [t] (and (vector? t) (= (count t) 4)
           (nat-int? (nth t 1))
           (nat-int? (nth t 2))
           (seq? (nth t 3))))

(example (suspension? ['(S O) 1 0 '()]) => true)

;;{
;; # Type syntax
;;
;; A type is either
;; - a type variable (identified by an capitalized symbol)
;; - the primitive "nat" type `i`
;; - the primitive "prop" type `o`
;; - the primitive "string" type `string`
;; - a user type
;; - a n-ary arrow type
;;}

(defn type-var?
  "Is `t` a type variable ?"
  [t] (and (symbol? t) (= (symbol (str/capitalize t)) t)))

(example (type-var? 'A) => true)

(defn nat-type?
  "Is `t` the nat type ?"
  [t] (= t 'i))

(example (nat-type? 'i) => true)

(defn prop-type?
  "Is `t` the proposition type ?"
  [t] (= t 'o))

(example (prop-type? 'o) => true)

(defn string-type?
  "Is `t` the string type ?"
  [t] (= t 'string))

(example (string-type? 'string) => true)

(defn user-type?
  "Is `t` a user type ?"
  [t] (and (symbol? t)
           (= (symbol (str/lower-case t)) t)))

(example (user-type? 'nat) => true)

(defn arrow-type?
  "Is `t` an arrow type ?"
  [t] (and (seq? t) (= (first t) '->)))

(examples
 (arrow-type? '(-> i o)) => true
 (arrow-type? '(-> ty1 ty2 ty1)) => true)

(defn applied-type-constructor?
  "Is `t` and applied constructor ?"
  [t] (and (seq? t) (user-type? (first t)) (not (empty? (rest t)))))

(example (applied-type-constructor? '(list nat)) => true)

(defn param-types
  "Get the parameter types of `ty` if it is an arrow type, and empty list if not"
  [ty] (if (arrow-type? ty) (take (- (count ty) 2) (rest ty)) '()))

(defn return-type
  "Get the return type of `ty` if it is an arrow type, and simply `ty` if not"
  [ty] (if (arrow-type? ty) (last ty) ty))

(defn flatten-arrow
  "Flatten an arrow type `ty` by right associativity"
  [ty] (if (arrow-type? ty)
         (let [f (take (- (count ty) 1) ty)
               l (flatten-arrow (last ty))]
           (if (arrow-type? l) (concat f (rest l)) (concat f (list l))))
         ty))

(defn flatten-one-arrow
  "Eliminate the '->' of an arrow `ty` if it is of length 1"
  [ty] (if (and (arrow-type? ty) (= (count ty) 2))
         (flatten-one-arrow (second ty)) ty))

(defn destruct-arrow
  "Get the `n` first parameter of an arrow type `ty`"
  [ty n] (if (= n 0) ['() (flatten-one-arrow ty)]
             (if (arrow-type? ty)
               (let [head (take n (rest ty)) body (nthrest ty (inc n))]
                 [head (if (> (count body) 1)
                         (cons '-> body) (first body))]))))

(defn normalize-ty
  "Put `ty` into normal form"
  [t] (flatten-one-arrow (flatten-arrow t)))

(defn curry-arrow
  "Curry an arrow ty `ty`, that is add parenthesis after the `n` first elements"
  [ty n] (if (arrow-type? ty)
           (let [heads (take (inc n) ty)]
             (concat heads (list (cons '-> (nthrest ty (inc n))))))))

;;{
;; # Prolog clause syntax
;;
;; A clause body is either:
;; - An applied predicate
;; - A pi abstraction : Π (x :> ty) body
;; - An implication : p => body
;;}

(defn pred?
  "Is `t` a predicate (either defined or free variable) ?"
  [t] (symbol? t))

(example (pred? 'even) => true)

(defn applied-pred?
  "Is `t` an applied predicate ?"
  [t] (and (seq? t) (pred? (first t))))

(example (applied-pred? '(even (S N))) => true)

(defn imp?
  "Is `t` an implication ?"
  [t] (and (seq? t) (> (count t) 2) (= '=> (first t))))

(example (imp? '(=> (infer x A) (infer (M x) B))) => true)

(defn pi?
  "Is `t` a pi-abstration ?"
  [t] (and (seq? t) (> (count t) 2) (= 'Π (first t))))

(example (pi? '(Π (x :> term) (=> (infer x A) (infer (M x) B)))) => true)

(defn clause-body?
  "Is `t` a clause body ?"
  [t] (and (seq? t) (not (empty? t))))

(example (clause-body? '((even N) (even O))) => true)

(defn clause?
  "Is `t` a clause ?"
  [t] (and (seq? t)
           (applied-pred? (first t))
           (or (empty? (rest t)) (= (second t) ':-))))

(examples
 (clause? '((even O))) => true
 (clause? '((even (S N)) :- (even N))) => true)
