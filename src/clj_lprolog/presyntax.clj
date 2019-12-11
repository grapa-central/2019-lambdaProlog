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

(defn bound?
  "Is `t` a bound variable ?"
  [t] (and (symbol? t)
           (not (some #{t} syn/reserved))
           (not= (symbol (str/capitalize t)) t)))

(defn free?
  "Is `t` a free variable ?"
  [t] (and (symbol? t)
           (not (some #{t} syn/reserved))
           (= (symbol (str/capitalize t)) t)))

(defn lambda?
  "Is `t` a λ-abstraction ?"
  [t] (and (seq? t)
           (= (first t) 'λ)
           (vector? (second t)) (every? bound? (second t))
           ;; (user-term? (second (rest t))) Too costly during parsing
           ))

(defn application?
  "Is `t` an application ?"
  [t] (and (seq? t)
           (not (empty? t)) (not (empty? (rest t)))
           ;; (every? user-term? t) ;; Too costly during parsing
           ))

(defn parse-aux
  "Parse a user term `t` to a kernel term using a naming environment"
  [t env]
  (cond (bound? t) (get (first env) t)
        (free? t) t
        (syn/primitive? t) t
        (lambda? t) (list
                     'λ
                     (count (second t))
                     (parse-aux (nth t 2)
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
  "Is `t` a predicate ?"
  [t] (and (symbol? t) (not= (symbol (str/capitalize t)) t)))

(defn applied-pred?
  "Is `t` an applied predicate ?"
  [t] (and (seq? t) (pred? (first t))
           ;; (every? syn/user-term? (rest t)) ;; Lets parse instead
           ))

(defn parse-applied-pred
  "Parse the arguments of an applied predicate `p`"
  [p] (cons (first p) (map parse (rest p))))

(defn clause-body?
  "Is `t` a clause body ?"
  [t] (or (every? applied-pred? t)))

(defn clause?
  "Is `t` a clause ?"
  [t] (and (seq? t)
           (applied-pred? (first t))
           (or (clause-body? (rest t))
               (and (= (second t) ':-) (clause-body? (nthrest t 2))))))

(defn parse-clause
  "Parse the clause `c` (removes the :-)"
  [c] (if (clause-body? (rest c))
        (map parse-applied-pred (cons (first c) (rest c)))
        (map parse-applied-pred (cons (first c) (nthrest c 2)))))

;; Contains the set of predicates during execution of the program !
;; (maybe not ideal, see with a clojure expert how to do it better)
(def progpreds (atom {}))

(defmacro defpred
  "Define a predicate. `n` is the name of the predicate, and `t` its type
  Also checks that the predicate is well formed. If it's not, return nil"
  [n t]
  `(if (and (pred? ~n) (syn/proper-type? ~t))
     (swap! progpreds (fn [pp#] (assoc pp# ~n [~t {}])))))

(defmacro addclause
  "Add `clause` to a predicate.
   Also checks that the clause is well formed"
  [clause]
  `(if (clause? ~clause)
     (let [clause# (parse-clause ~clause)
           head# (first clause#)
           body# (rest clause#)]
       (swap! progpreds
              (fn [pp#]
                (if-let [prev# (get @progpreds (first head#))]
                  (assoc pp# (first head#)
                         (list (first prev#)
                               (assoc (second prev#)
                                      head# body#)))))))))
