(ns clj-lprolog.norm
  "Kernel normalization"
  (:require [clj-lprolog.utils :as u :refer [example examples]]
            [clj-lprolog.syntax :as syn]
            [clj-lprolog.typecheck :as typ]
            [clojure.test :as t]))

(def +examples-enabled+ true)

;;{
;; # Suspension calculus
;;
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
                     [(with-meta
                        (syn/flatten-zero-lambda
                         (list 'λ (dec (second abs)) t1))
                        {:ty (second (syn/destruct-arrow (typ/type-of abs) 1))})
                      ol (dec nl) (cons [t2 (dec nl)] (rest e))])
                   ;; else
                   [(syn/flatten-zero-lambda
                     (with-meta
                       (list 'λ (dec (second abs)) (nth abs 2))
                       {:ty (second (syn/destruct-arrow (typ/type-of abs) 1))}))
                    1 0 (list [t2 0])]
                   )]
          (with-meta (if (empty? tl) hd (cons hd tl)) {:ty (typ/type-of t)}))))

(examples
 (beta-step '((λ 1 #{0}) A)) => '[#{0} 1 0 ([A 0])]
 (beta-step '((λ 2 #{0}) A B)) => '([(λ 1 #{0}) 1 0 ([A 0])] B)
 (beta-step '((λ 2 [A 2 3 (2 [#{0} 1])]) B C)) =>
 '([(λ 1 A) 2 2 ([B 2] [#{0} 1])] C))

(defn rewrite-suspension
  "Apply the suspension rewriting rules. Assumes `s` is a suspension"
  [s] (let [[t ol nl e] s]
        (with-meta
          (cond
            ;; r1 : t is a constant
            (syn/primitive? t) t
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
            (let [[t ol nl' e] t] [t ol (+ nl nl') e])
            ;; r9
            (and (zero? ol) (zero? nl) (empty? e)) t)
        {:ty (typ/type-of t)})))

(examples
 (rewrite-suspension ['(t1 t2) 'ol 'nl 'e]) => '([t1 ol nl e] [t2 ol nl e])
 (rewrite-suspension ['(λ 2 A) 1 1 '()]) => '(λ 2 [A 3 3 (2 1)]))

(defn apply-suspensions
  "Recursively apply all the suspensions left in `t`"
  [t]
  (with-meta
    (cond
      (syn/lambda? t) (list 'λ (second t) (apply-suspensions (nth t 2)))
      (syn/application? t) (map apply-suspensions t)
      (syn/suspension? t) (apply-suspensions (rewrite-suspension t))
      :else t) {:ty (typ/type-of t)}))

(defn reduce
  "Fully beta reduce `t` using explicit substitutions"
  [t]
  (loop [t t]
    (cond
      (syn/beta-redex? t) (recur (beta-step t))
      (and (syn/application? t) (syn/suspension? (first t)))
      (recur (with-meta
               (cons (rewrite-suspension (first t)) (rest t))
               {:ty (typ/type-of t)}))
      :else (apply-suspensions t))))

(examples
 (reduce '((λ 1 #{0}) A)) => 'A
 (reduce '((λ 1 #{0}) A B)) => '(A B)
 (reduce '((λ 2 #{0}) A B)) => 'B)
