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
           (nat-int? (second t))))

(defn proper-lambda?
  "Is `t` a well-formed λ-abstraction ?"
  [t] (and (lambda? t)
           (kernel-term? (nth t 2))))

(defn application?
  "Is `t` an application ?"
  [t] (and (seq? t)
           (not (empty? t)) (not (empty? (rest t)))))

(defn proper-application?
  "Is `t` a well-formed application ?"
  [t] (and (application? t)
           (every? kernel-term? t)))

(defn kernel-term?
  "Is `t` a kernel term ?"
  [t] (or (bound? t)
          (free? t)
          (primitive? t)
          (proper-lambda? t)
          (proper-application? t)))

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
           (= (first t) '->)))

(defn proper-arrow-type?
  "Is `t` an arrow type ?"
  [t] (and (arrow-type? t)
           (every? proper-type? (rest t))))

(defn proper-type?
  "Is `t` a proper type ?"
  [t] (or (type-var? t)
          (nat-type? t)
          (prop-type? t)
          (proper-arrow-type? t)))

(defn destruct-arrow
  "Get the `n` first parameter of an arrow type `ty`"
  [ty n] (if (arrow-type? ty)
           (let [head (take n (rest ty)) body (nthrest ty (inc n))]
             [head (if (> (count body) 1)
                     (cons '-> body) (first body))])))

(defn flatten-arrow
  "Flatten an arrow type `ty` by right associativity"
  [ty] (if (arrow-type? ty)
           (let [f (take (- (count ty) 1) ty)
                 l (flatten-arrow (last ty))]
             (if (arrow-type? l) (concat f (rest l)) (concat f (list l))))
           ty))

(defn curry-arrow
  "Curry an arrow ty `ty`, that is add parenthesis after the `n` first elements"
  [ty n] (if (arrow-type? ty)
           (let [heads (take (inc n) ty)]
             (concat heads (list (cons '-> (nthrest ty (inc n))))))))
