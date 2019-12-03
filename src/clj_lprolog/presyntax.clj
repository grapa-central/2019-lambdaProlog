(ns clj-lprolog.presyntax
  "User lambda-calculus syntax"
  (:require [clojure.string :as str]
            [clj-lprolog.syntax :as syn]))

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

(declare user-term?)

(defn bound?
  "Is `t` a bound variable ?"
  [t] (and (symbol? t)
           (not= (symbol (str/capitalize t)) t)))

(defn free?
  "Is `t` a free variable ?"
  [t] (and (symbol? t)
           (= (symbol (str/capitalize t)) t)))

;; TODO primitive constants

(defn lambda?
  "Is `t` a λ-abstraction ?"
  [t] (and (seq? t)
           (= (first t) 'λ)
           (vector? (second t)) (every? bound? (second t))
           (user-term? (second (rest t)))))

(defn application?
  "Is `t` an application ?"
  [t] (and (seq? t)
           (not (empty? t)) (not (empty? (rest t)))
           (every? user-term? t)))

(defn user-term?
  "Is `t` a kernel term ?"
  [t] (or (bound? t)
          (free? t)
          (lambda? t)
          (application? t)))

(defn parse-aux
  "Parse a user term `t` to a kernel term using a naming environment"
  [t env]
  (cond (bound? t) (get (first env) t)
        (free? t) t
        (lambda? t) (list
                     'λ
                     (count (second t))
                     (parse-aux (second (rest t))
                                (reduce (fn [e x]
                                          (list
                                           (assoc (first e) x (second e))
                                           (+ (second e) 1)))
                                        env (second t))))
        (application? t) (map (fn [t] (parse-aux t env)) t)))

(defn parse
  "Parse a user term `t` to a kernel term.
  Mainly transforms bound variables to De Bruijn indices"
  [t] (let [t' (parse-aux t '({} 0))]
        (if (syn/kernel-term? t') t' nil)))

(parse '(λ [x y] (x y Z)))
