(ns clj-lprolog.syntax
  "Kernel syntax"
  (:require [clojure.string :as str]))

;;{
;; # Kernel terms
;;
;; A kernel lambda-term is either:
;; - a bound variable (identified by its De Bruijn index)
;; - a free (substituable) variable (identified by a symbol)
;; - a primitive constant
;; - a n-ary λ-abstraction
;; - a n-ary application
;;}

(declare kernel-term?)

(defn bound?
  "Is `t` a bound variable ?"
  [t] (nat-int? t))

(defn free?
  "Is `t` a free variable ?"
  [t] (symbol? t))

;; TODO primitive constants (true and false ? Or arithmetics ?)

(defn lambda?
  "Is `t` a λ-abstraction ?"
  [t] (and (seq? t)
           (= (first t) 'λ)
           (nat-int? (second t))
           (kernel-term? (second (rest t)))))

(defn application?
  "Is `t` an application ?"
  [t] (and (seq? t)
           (not (empty? t)) (not (empty? (rest t)))
           (every? kernel-term? t)))

(defn kernel-term?
  "Is `t` a kernel term ?"
  [t] (or (bound? t)
          (free? t)
          (lambda? t)
          (application? t)))

;;{
;; # Type syntax
;;
;; A type is either
;; - a type variable (identified by an capitalized symbol)
;; - the primitive "prop" type `o`
;; - a n-ary arrow type
;;}

(declare proper-type?)

(defn type-var?
  "Is `t` a type variable ?"
  [t] (and (symbol? t)
           (= (symbol (str/capitalize t)) t)))

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
          (prop-type? t)
          (arrow-type? t)))
