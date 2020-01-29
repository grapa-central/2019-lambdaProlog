(ns clj-lprolog.norm
  "Kernel normalization"
  (:require [clj-lprolog.utils :as u :refer [example examples]]
            [clj-lprolog.syntax :as syn]
            [clj-lprolog.typecheck :as typ]
            [clojure.test :as t]))

(def +examples-enabled+ true)

;;{
;; # Syntax transformations
;;
;; A few transformations for abstractions and applications.
;; All these transformations preserve type information
;;}

(defn flatten-zero-lambda
  "Transform `t`, a lambda into its body if the lambda abstract over 0 bindings.
  Return unchanged `t` otherwise"
  [t] (typ/set-type
        (if (zero? (second t)) (nth t 2) t)
        (typ/type-of t)))

(defn flatten-lambda
  "Flatten `t`, with `t` some nested lambdas (at least one)"
  [t] (typ/set-type
        (if (syn/lambda? (nth t 2))
        (let [lam' (flatten-lambda (nth t 2))
              n' (second lam')
              t' (nth lam' 2)]
          (list 'λ (+ (second t) n') t'))
        t)
        (typ/type-of t)))

(example
 (flatten-lambda '(λ 2 (λ 1 (#{0} #{1})))) =>
 '(λ 3 (#{0} #{1})))

(defn lambda-form
  "Put a term `t` in lambda form: if it is already a lambda, return `t`,
  else add an abstraction with 0 bindings"
  [t] (typ/set-type
        (if (syn/lambda? t) t (list 'λ 0 t))
        (typ/type-of t)))

(example (lambda-form '(A B)) => '(λ 0 (A B)))

(defn flatten-one-application
  "Transform `t`, an application into its head if it is applied to 0 arguments.
  Return unchanged `t` otherwise"
  [t] (typ/set-type
        (if (= (count t) 1) (first t) t)
        (typ/type-of t)))

(defn flatten-application
  "Flatten `t` using left associativity,
  with `t` some nested applications (at least one)"
  [t] (typ/set-type
        (if (syn/application? (first t))
          (let [hd (flatten-application (first t))]
            (concat hd (rest t)))
          t)
        (typ/type-of t)))

(example
 (flatten-application '((A B C) D E)) => '(A B C D E))

(defn application-form
  "Put a term `t` in an application form : if it is already an application,
  returnn `t`, else apply it to 0 arguments"
  [t] (typ/set-type
        (if (syn/application? t) t (list t))
        (typ/type-of t)))

(example (application-form 'A) => '(A))

;;{
;; # Reduction with implicit substitutions
;;
;; First version of beta reduction, using simple implicit substitutions.
;; To be improved later
;;}

(defn one-less-lambda
  "Remove the first binding of `t`, a lambda. Flatten if there is no binding left.
   Update typing information"
  [t] (typ/set-type
        (flatten-zero-lambda
         (list 'λ (dec (second t)) (nth t 2)))
        (second (syn/destruct-arrow (typ/type-of t) 1))))

(defn implicit-subst
  "Substitute `v` to the bound variable of index `n` (default to 0) in `t`"
  ([v t] (implicit-subst 0 v t))
  ([n v t] (typ/set-type
            (cond
              ;; t is a bound variable
              (and (syn/bound? t) (= n (first t))) v
              ;; t is an abstraction
              (syn/lambda? t)
              (list 'λ (second t) (implicit-subst (+ n (second t)) v (nth t 2)))
              ;; t is an application
              (syn/application? t)
              (map (fn [t] (implicit-subst n v t)) t)
              :else t)
            (typ/type-of t))))

(example
 (implicit-subst 'A '((λ 2 #{2}) #{0} #{1})) => '((λ 2 A) A #{1}))

(defn implicit-reduce
  "Fully beta-reduce `t` using implicit substitutions"
  [t]
  (loop [t t]
    (if (syn/beta-redex? t)
      (let [hd (first t)
            arg (second t) tl (rest (rest t))
            newhd (implicit-subst arg (one-less-lambda hd))]
        (recur (flatten-one-application (typ/set-type
                                          (cons newhd tl)
                                          (typ/type-of t)))))
      t)))

(example
 (implicit-reduce '((λ 2 #{0}) A B)) => 'B)

;;{
;; # Reduction with explicit substitutions

;; Reduction with explicit substitutions
;; Adapted from "Explicit Substitutions in the Reduction of Lambda Terms",
;; Gopalan Nadathur and Xiaochu Qui, PPDP 2003
;;}

(defn beta-step
  "Beta-reduce `t`, assuming it is a beta-redex"
  [t] (let [abs (first t)
            t2 (second t)
            tl (nthrest t 2)]
        (let [hd (if (syn/suspension? (nth abs 2))
                   ;; if the body of the abstraction is already a suspension
                   (let [[t1 ol nl e] (nth abs 2)]
                     [(typ/set-type
                        (flatten-zero-lambda
                         (list 'λ (dec (second abs)) t1))
                        (second (syn/destruct-arrow (typ/type-of abs) 1)))
                      ol (dec nl) (cons [t2 (dec nl)] (rest e))])
                   ;; else
                   [(typ/set-type
                      (flatten-zero-lambda
                       (list 'λ (dec (second abs)) (nth abs 2)))
                       (second (syn/destruct-arrow (typ/type-of abs) 1)))
                    1 0 (list [t2 0])]
                   )]
          (typ/set-type (if (empty? tl) hd (cons hd tl)) (typ/type-of t)))))

(examples
 (beta-step '((λ 1 #{0}) A)) => '[#{0} 1 0 ([A 0])]
 (beta-step '((λ 2 #{0}) A B)) => '([(λ 1 #{0}) 1 0 ([A 0])] B)
 (beta-step '((λ 2 [A 2 3 (2 [#{0} 1])]) B C)) =>
 '([(λ 1 A) 2 2 ([B 2] [#{0} 1])] C))

(defn rewrite-suspension
  "Apply the suspension rewriting rules. Assumes `s` is a suspension"
  [s] (let [[t ol nl e] s]
        (typ/set-type
          (cond
            ;; r9
            (and (zero? ol) (zero? nl) (empty? e)) t
            ;; r1 : t is a constant
            (or (syn/primitive? t) (syn/user-const? t)) t
            ;; r2 : t is an instantiatable variable
            (syn/free? t) t
            ;; r3 : i > ol and j = i - ol + nl
            (and (syn/bound? t) (> (first t) ol))
            #{(+ (- (first t) ol) nl)}
            ;; r4 : i <= ol, e[i] = @l and j = nl - l
            (and (syn/bound? t) (<= (first t) ol) (nat-int? (nth e (first t))))
            #{(- nl (nth e (first t)))}
            ;; r5 : i <= ol, e[i] = (t, l) and j = nl - l
            (and (syn/bound? t) (<= (first t) ol) (vector? (nth e (first t))))
            [(first (nth e (first t))) 0 (- nl (second (nth e (first t)))) '()]
            ;; r6 : t is an application
            (syn/application? t) (map (fn [t] [t ol nl e]) t)
            ;; r7 : t is an abstraction
            (syn/lambda? t)
            (list 'λ (second t)
                  [(nth t 2) (+ ol (second t)) (+ nl (second t))
                   (concat (reverse (take (second t) (iterate inc nl))) e)])
            ;; r8 : t is a suspension
            (and (syn/suspension? t) (zero? ol) (empty? e))
            (let [[t ol nl' e] t] [t ol (+ nl nl') e]))
          (typ/type-of t))))

(examples
 (rewrite-suspension ['(t1 t2) 2 2 '()]) => '([t1 2 2 ()] [t2 2 2 ()])
 (rewrite-suspension ['(λ 2 A) 1 1 '()]) => '(λ 2 [A 3 3 (2 1)]))

(defn apply-suspensions
  "Recursively apply all the suspensions left in `t`"
  [t]
  (typ/set-type
    (cond
      (syn/application? t) (map apply-suspensions t)
      (syn/suspension? t) (apply-suspensions (rewrite-suspension t))
      :else t) (typ/type-of t)))

(defn explicit-reduce
  "Fully beta reduce `t` using explicit substitutions"
  [t]
  (loop [t t]
    (cond
      (syn/beta-redex? t) (recur (beta-step t))
      (syn/suspension? t) (recur (rewrite-suspension t))
      (and (syn/application? t) (syn/suspension? (first t)))
      (recur (typ/set-type
               (cons (rewrite-suspension (first t)) (rest t))
               (typ/type-of t)))
      :else (apply-suspensions t))))

(examples
 (explicit-reduce '((λ 1 #{0}) A)) => 'A
 (explicit-reduce '((λ 1 #{0}) A B)) => '(A B)
 (explicit-reduce '((λ 2 #{0}) A B)) => 'B)

;;{
;; # Normalization
;;
;; Beta-reduce and eta-expand a term to get its head-normal form
;;}

(defn literal?
  "Check if `t` is a litteral"
  [t] (or (syn/string-lit? t) (syn/int-lit? t)))

(example (literal? 42) => true)

(defn evaluable?
  "Check if `t` is an evaluable term, that is the application of a primitive to a set of literals"
  [t] (and (seq? t) (not (syn/arrow-type? (typ/type-of t)))
           (syn/primitive? (first t))
           (every? (fn [t] (or (literal? t) (evaluable? t))) (rest t))))

(example (evaluable? '(+ 2 (* 4 3))) => true)

(defn simplify-term
  "Recursively simplify a term as much as possible"
  [t] (typ/set-type
        (cond
          ;; t is a beta-redex
          (syn/beta-redex? t) (simplify-term (implicit-reduce t))
          ;; t can be evaluated
          (evaluable? t) (eval t)
          ;; t is an abstraction
          (syn/lambda? t)
          (flatten-zero-lambda
           (flatten-lambda (list 'λ (second t) (simplify-term (nth t 2)))))
          ;; t is an application
          (syn/application? t)
          (flatten-one-application
           (flatten-application (map simplify-term t)))
          :else t)
        (typ/type-of t)))

(example (simplify-term '(λ 0 ((λ 0 ((λ 0 (S))))))) => 'S)

(defn norm-beta
  "Beta-reduction part of the normalization of `t`"
  [t] (-> t
          implicit-reduce ;; TODO use explicit-reduce ?
          lambda-form flatten-lambda
           (#(typ/set-type (list 'λ (second %) (application-form (nth % 2)))
                           (typ/type-of %)))
          ))

(defn lift-indices
  "Lift indices of all free variables in `t` by `n`"
  ([n t] (lift-indices n 0 t))
  ([n nmin t]
   (typ/set-type (cond
                   ;; t is a bound variable, but not by a later abstraction
                   (and (syn/bound? t) (>= (first t) nmin)) #{(+ (first t) n)}
                   ;; t is an abstraction
                   (syn/lambda? t)
                   (list 'λ (second t)
                         (lift-indices n (+ nmin (second t)) (nth t 2)))
                   ;; t is an application
                   (syn/application? t) (map (fn [t] (lift-indices n nmin t)) t)
                   :else t)
                 (typ/type-of t))))

(example (lift-indices 2 '((λ 2 (#{0} #{2})) #{1}))
         => '((λ 2 (#{0} #{4})) #{3}))

(defn eta-params
  "Generate the parameters for the application of a constructor of type `ty`,
  in reverse order"
  [ty] (let [n (count (syn/param-types ty))]
         (take n (iterate
                  (fn [s] (typ/set-type #{(dec (first s))}
                                       (nth ty (- n (dec (first s))))))
                  (typ/set-type #{(dec n)} (nth ty 1))))))

(defn norm-eta
  "Eta-expansion part of the normalization of `t`"
  [t] (let [ty (typ/type-of (nth t 2))]
        (if (syn/arrow-type? ty)
          (let [n (- (count ty) 2)
                args (eta-params ty)]
            (typ/set-type (list 'λ (+ n (second t))
                                (typ/set-type
                                 (flatten-application
                                  (cons (lift-indices n (nth t 2)) args))
                                 (last ty)))
                          (typ/type-of t)))
          t)))

(example
 (norm-eta (second (typ/elaborate-term {} '(λ 1 (λ 2 (S #{0}))))))
 => '(λ 3 ((λ 2 (S #{0})) #{1} #{0})))

(defn normalize
  "Normalize `t`, using eta-expansion and beta-reduction"
  [t] (-> t simplify-term norm-beta norm-eta))

(example (normalize '((λ 1 (λ 1 #{1})) (λ 1 (A #{0})))) => '(λ 2 (A #{0})))
