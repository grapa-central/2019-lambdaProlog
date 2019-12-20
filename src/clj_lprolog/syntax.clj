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
;; - a primitive constant (∀, =>, O, S, +, *)
;; - a n-ary λ-abstraction
;; - a n-ary application
;;}

(def reserved
  "Reserved symbols"
  '(λ ∀ => O S + *))

(defn bound?
  "Is `t` a bound variable ?"
  [t] (nat-int? t))

(example (bound? 1) => true)

(defn free?
  "Is `t` a free variable ?"
  [t] (and (symbol? t) (not (some #{t} reserved))))

(example (free? 'A) => true)

(defn primitive?
  "Is `t` a primitive constant ?"
  [t] (some? (some #{t} (nthrest reserved 2))))

(examples
 (primitive? 'S) => true
 (primitive? '+) => true)

(defn lambda?
  "Is `t` a λ-abstraction ?"
  [t] (and (seq? t)
           (= (first t) 'λ)
           (nat-int? (second t))))

(example (lambda? '(λ 2 (+ 1 0))) => true)

(defn application?
  "Is `t` an application ?"
  [t] (and (seq? t)
           (not (empty? t)) (not (empty? (rest t)))))

(example (application? '(S O)) => true)

;;{
;; # Type syntax
;;
;; A type is either
;; - a type variable (identified by an capitalized symbol)
;; - the primitive "nat" type `i`
;; - the primitive "prop" type `o`
;; - a n-ary arrow type
;;}

(defn type-var?
  "Is `t` a type variable ?"
  [t] (and (symbol? t)
           (= (symbol (str/capitalize t)) t)))

(example (type-var? 'A) => true)

(defn nat-type?
  "Is `t` the nat type ?"
  [t] (= t 'i))

(example (nat-type? 'i) => true)

(defn prop-type?
  "Is `t` the proposition type ?"
  [t] (= t 'o))

(example (prop-type? 'o) => true)

(defn arrow-type?
  "Is `t` an arrow type ?"
  [t] (and (seq? t)
           (> (count t) 2)
           (= (first t) '->)))

(examples
 (arrow-type? '(-> i o)) => true
 (arrow-type? '(-> ty1 ty2 ty1)) => true)

(defn destruct-arrow
  "Get the `n` first parameter of an arrow type `ty`"
  [ty n] (if (= n 0) ['() ty]
             (if (arrow-type? ty)
               (let [head (take n (rest ty)) body (nthrest ty (inc n))]
                 [head (if (> (count body) 1)
                         (cons '-> body) (first body))]))))

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
