(ns clj-lprolog.unif
  "Lambda term unification"
  (:require [clojure.set]
            [clj-lprolog.utils :as u :refer [example examples ok>]]
            [clj-lprolog.syntax :as syn]
            [clj-lprolog.typecheck :as typ]
            [clj-lprolog.norm :as nor]))

(def +examples-enabled+ true)

;;{
;; Utilities
;;}

(defn get-freevars
  "Get the set of free variables in `t`"
  [t] (cond
        (syn/free? t) #{t}
        (syn/lambda? t) (get-freevars (nth t 2))
        (syn/application? t)
        (reduce (fn [s t] (clojure.set/union s (get-freevars t))) #{} t)
        :else #{}))

(example (get-freevars '((λ 2 (A B)) C B)) => '#{A B C})

;;{
;; # Substitution utilities
;;
;; Similar to the ones used in the typechecker
;;}

(defn subst
  "Substitute a term `v` to the unification variable `var` in `t`"
  [var v t] (do (typ/set-type
              (cond
                ;; t is the right unification variable
                (and (syn/free? t) (= var t)) v
                ;; t is an abstraction
                (syn/lambda? t)
                (list 'λ (second t) (subst var v (nth t 2)))
                ;; t is an application
                (syn/application? t)
                (map (fn [t] (subst var v t)) t)
                ;; otherwise (primitive or bound variable)
                :else t)
              (typ/type-of t))))

(example
 (subst 'A 42 '((λ 2 A) A B)) => '((λ 2 42) 42 B))

(defn apply-subst
  "Apply a substitution `si` to a type `ty`"
  [si term] (reduce (fn [term [var t]] (subst var t term)) term si))

(defn subst-clash?
  "Check if there is a clash between `s1` and `s2`"
  [s1 s2] (some (fn [[x y1]]
                  (if (contains? s2 x)
                    (let [y2 (get s2 x)]
                      (and (not (= y1 y2))
                           (not (or (syn/free? y1)
                                    (syn/free? y2))))))) s1))

(defn apply-subst-subst
  "Apply `s1` to every value of `s2`"
  [s1 s2] (u/map-of-pair-list
           (map (fn [[k term]] [k (nor/simplify-term (apply-subst s1 term))]) s2)))

(defn compose-subst
  "Compose two substitutions `s1` and `s2`, after checking that they dont clash"
  [s1 s2]
  (ok> (when (subst-clash? s1 s2) [:ko> 'subst-clash {:s1 s1 :s2 s2}])
       (conj (apply-subst-subst s2 s1) (apply-subst-subst s1 s2)) :as subst
       [:ok subst]))

(defn compose-subst-sets
  "Compose the sets of substitutions `s1` and `s2`.
  Keep all of the non-clashing substitution pair"
  [s1 s2]
  (set (map
        (fn [[_ s]] s)
        (filter u/ok-expr?
                (reduce (fn [se s1] (clojure.set/union
                                    se
                                    (map (fn [s2] (compose-subst s1 s2)) s2)))
                        #{} s1)))))

(example
 (compose-subst-sets '#{{A a} {B b}} '#{{A a} {B a}})
 => '#{{A a} {A a B a} {B b A a}})

(defn occur-check-substs
  "Filter the substitutions in `substs`, keeping only the ones 
  without occur-check"
  [substs]
  (set (filter
        (fn [si] (not (some (fn [[v t]]
                             (and (not= v t)
                                  (some #{v} (get-freevars t)))) (seq si))))
        substs)))

;;{
;; # First order unification
;;
;; With first order terms, we can simply use a first order unification algorithm
;; which is less costly than Huet's algorithm
;;}

(defn first-order-term?
  "Check if `t` is a first-order term, in which case the first-order
  unification algorithm will be applicable"
  [t] (cond
        (syn/lambda? t) false
        (syn/application? t) (every? first-order-term? t)
        :else true))

(example (first-order-term? '(cs A B)) => true)

(defn mgu-first-order
  "Returns a unifying substitution of `t1` and `t2`, two first order terms"
  [t1 t2]
  ((fn aux [ts si]
     (if (empty? ts) [:ok si]
         (let [[t1 t2] (first ts)
               t1 (nor/simplify-term (apply-subst si t1))
               t2 (nor/simplify-term (apply-subst si t2))]
           (cond
             ;; The two terms are the same
             (= t1 t2) (recur (rest ts) si)

             ;; t1 is a unification variable
             (syn/free? t1)
             (ok>
              (when (some #{t1} (get-freevars t2))
                [:ko 'occur-check {:t1 t1 :t2 t2}])
              (compose-subst si {t1 t2}) :as [_ si]
              [:ko> 'mgu-first-order {:t1 t1 :t2 t2}]
              (aux (rest ts) si))

             ;; t2 is a unification variable
             (syn/free? t2)
             (ok>
              (when (some #{t2} (get-freevars t1))
                [:ko 'occur-check {:t1 t2 :t2 t1}])
              (compose-subst si {t2 t1}) :as [_ si]
              [:ko> 'mgu-first-order {:t1 t1 :t2 t2}]
              (aux (rest ts) si))

             ;; t1 and t2 are applications
             (and (syn/application? t1) (syn/application? t2)
                  (= (count t1) (count t2)))
             (recur (concat (map (fn [x y] [x y]) t1 t2)
                            (rest ts)) si)

             ;; terms are not unifiable
             :else [:ko 'not-unifiable {:t1 t1 :t2 t2}]
             )))) (list [t1 t2]) {}))

(defn first-order-unify
  "Returns a set containing the best unifying substitution of `t1` and `t2`,
  two first order terms"
  [t1 t2] (ok> (mgu-first-order t1 t2) :as [_ si]
               [:ko> 'first-order-unify {:t1 t1 :t2 t2}]
               [:ok #{si}]))

(example (first-order-unify '(A one ni) '(cs B ni)) => [:ok '#{{A cs B one}}])

;;{
;; # High order unification
;;
;; Based on Huet's algorithm
;; The unification variables are the free variables
;;}

(defn head
  "Get the head of the application under the abstraction of `t`"
  [t] (first (nth t 2)))

(defn tail
  "Get the arguments of the application under the abstraction of `t`"
  [t] (rest (nth t 2)))

(defn flexible?
  "Is `t` flexible ?"
  [t] (syn/free? (head t)))

(defn rigid?
  "Is `t` rigid ?"
  [t] (not (flexible? t)))

(defn unif-var?
  "Is `t` a unification variable in head normal form (under an arity-0 lambda)"
  [t] (and (zero? (second t)) (= 1 (count (nth t 2))) (syn/free? (head t))))

(example (unif-var? '(λ 0 (C))) => true)

(defn trivial-substitutions
  "Get the trivial substitutions from the pairs containing a unification var"
  [pairs]
  (u/ok-reduce
   (fn [subst [t1 t2]]
     (cond
       (and (unif-var? t1) (not (contains? (get-freevars t2) (head t1))))
       (compose-subst subst {(head t1) t2})
       (and (unif-var? t2) (not (contains? (get-freevars t1) (head t2))))
       (compose-subst subst {(head t2) t1})
       :else [:ok subst])) {} pairs))

(example (trivial-substitutions '([(λ 0 (A)) (λ 0 (S (S #{0})))]))
         => '[:ok {A (S (S #{0}))}])

(defn trivial
  "Trivial unification : find the pairs containing a unification var
   and substitute them"
  [pairs]
  (ok> (trivial-substitutions pairs) :as [_ subst]
       (filter
        (fn [[t1 t2]] (not= t1 t2))
        (map (fn [[t1 t2]] [(nor/normalize (apply-subst subst t1))
                           (nor/normalize (apply-subst subst t2))])
             pairs))))

(example (trivial '([(λ 0 (A)) (λ 0 (S))]  [(λ 0 (B)) (λ 0 (A O))]
                    [(λ 2 (C)) (λ 2 (A B))]))
         => '([(λ 2 (C)) (λ 2 (S (S O)))]))

(defn normal-form-arg
  "Put an arg `e` into normal form with `n` abstractions,
  keep the right metadata from `ty`"
  [e n ty] (typ/set-type (nor/normalize (list 'λ n e))
                         (cons '->
                               (concat (first (syn/destruct-arrow ty n))
                                       (list (typ/type-of e))))))

(example (normal-form-arg (typ/set-type 'O 'i) 2 '(-> i i o))
         => '(λ 2 (O)))

(defn simpl
  "Simplification : among a number of pairs, apply the trivial procedure,
  then decompose the head normal forms and
  unify the parameters of rigide-rigide pairs"
  ([pairs] (simpl pairs {}))
  ([pairs subst]
   (ok>
    (map (fn [[t1 t2]] [(nor/normalize t1) (nor/normalize t2)]) pairs) :as pairs
    (trivial-substitutions pairs) :as [_ subst2]
    (compose-subst subst subst2) :as [_ subst]
    (filter
     (fn [[t1 t2]] (not= t1 t2))
     (map (fn [[t1 t2]] [(nor/normalize (apply-subst subst t1))
                        (nor/normalize (apply-subst subst t2))])
          pairs)) :as pairs
    (if (or (empty? pairs)
            (every? (fn [[t1 t2]]
                      (or (flexible? t1) (flexible? t2))) pairs))
      [:ok pairs subst]
      (ok>
       (some
        (fn [[t1 t2]]
          (when (and (rigid? t1) (rigid? t2))) [t1 t2])
        pairs) :as [t1 t2]
       (u/remove-first
        (fn [[t1 t2]] (and (rigid? t1) (rigid? t2))) pairs) :as pairs
       (when (or (not= (second t1) (second t2))
                 (not= (head t1) (head t2)))
         [:ko 'not-unifiable {:t1 t1 :t2 t2}])
       (simpl
        (concat pairs
                (map
                 (fn [[e1 e2]]
                   [(normal-form-arg e1 (second t1) (typ/type-of t1))
                    (normal-form-arg e2 (second t2) (typ/type-of t2))])
                 (map vector (tail t1) (tail t2)))) subst))))))

(example (simpl '([(λ 0 (A)) (λ 0 (succ))] [(λ 2 (A (B))) (λ 2 (A #{1}))])) =>
         '[:ok ([(λ 2 (B)) (λ 2 (#{1}))]) {A succ}])

(defn fresh-unknown [count] (symbol (str "H" count)))

(defn eta-fresh-unknown
  "Generate an application of a fresh variable `i` to `k` arguments
  following the input type `ty` with return type `rty`"
  [i k ty rty] (typ/set-type
                 (cons (typ/set-type
                         (fresh-unknown i)
                         (concat
                          (take (inc k) ty)
                          (list rty)))
                       (nor/eta-params ty))
                 rty))

(defn match
  "Non deterministic Match procedure : takes a flexible-rigid pair <`e1`, `e2`>,
  and return a set of possible substitutions for unification,
  using imitation and projection"
  ([e1 e2] (first (match e1 e2 0)))
  ([e1 e2 cunknown]
   (ok> (head e1) :as v
        (typ/type-of v) :as vty
        (count (syn/param-types vty)) :as n
        ;; Imitation
        (if (or (syn/primitive? (head e2)) (syn/user-const? (head e2)))
          [#{{v
              (typ/set-type
                (list 'λ n
                      (typ/set-type
                        (cons (head e2)
                              (let [k (second e2)
                                    ty (typ/type-of (head e2))]
                                (map
                                 (fn [[e i]]
                                   (eta-fresh-unknown i k ty (typ/type-of e)))
                                 (map vector (tail e2)
                                      (take (count (tail e2))
                                            (iterate inc cunknown))))))
                        (typ/type-of (nth e2 2))))
                (typ/type-of v))}} (+ cunknown (count (tail e2)))]
          [#{} cunknown]) :as [imitation cunknown]
        ;; Projections
        (syn/return-type vty) :as beta
        (reduce
         (fn [[s cunknown] [i ty]]
           (ok>
            (clojure.set/union
             s #{{v
                  (typ/set-type
                    (list 'λ n
                          (typ/set-type
                            (cons (typ/set-type #{i} ty)
                                  (map (fn [[ty i]]
                                         (eta-fresh-unknown i n vty ty))
                                       (map vector (syn/param-types ty)
                                            (take (count (syn/param-types ty))
                                                  (iterate inc cunknown)))))
                            beta))
                    vty)}}) :as s
            [s (+ cunknown (count (syn/param-types ty)))]))
         [#{} cunknown]
         (filter (fn [[i ty]] (= beta (syn/return-type ty)))
                 (map (fn [i] [i (typ/type-of (nth (reverse (tail e1)) i))])
                      (take (count (tail e1)) (iterate inc 0)))))
        :as [projections cunknown]
        [(clojure.set/union imitation projections) cunknown])))

(defn find-flexible-rigid
  "Find a flexible-rigid pair in `pairs`"
  [pairs] (some (fn [[t1 t2]]
                  (cond
                    (and (flexible? t1) (rigid? t2)) [t1 t2]
                    (and (rigid? t1) (flexible? t2)) [t1 t2]
                    :else nil)) pairs))

(defn huet
  "Huet research procedure for unifying the pair <`t1`, `t2`>"
  ([pairs] (huet pairs {} 100)) ;; Low fuel for now
  ([pairs subst fuel]
   (ok>
    ;; Check the fuel
    (when (<= fuel 0) [:ko 'out-of-fuel])
    ;; Simplify the pair
    (map (fn [[t1 t2]] [(apply-subst subst t1)
                       (apply-subst subst t2)]) pairs) :as pairs
    (simpl pairs) :as [_ N subst2]
    (compose-subst subst subst2) :as [_ subst]
    (if (empty? N) [:ok #{subst}]
        ;; Find a flexible-rigid pair in N
        (if-let [[t1 t2] (find-flexible-rigid N)]
          (ok>
           (match t1 t2) :as substs
           (set (u/map-filter (fn [subst2]
                                (let [subst (compose-subst subst subst2)]
                                  (if (u/ok-expr? subst) (second subst))))
                              substs)) :as substs
           (u/map-filter
            (fn [subst]
              (let [v (huet N subst (dec fuel))]
                (if (u/ok-expr? v) (second v)))) substs) :as substs
           (reduce (fn [s1 s2] (clojure.set/union s1 s2))
                        #{} substs) :as substs
           [:ok substs])
          [:ok #{subst}]))
    )))

;;{
;; Top-level unification procedure
;;}

(defn unify
  "Unify `t1` and `t2` (return a set of unifying substitutions),
  using first-order unification if applicable and the high-order
  huet algorithm otherwise"
  [t1 t2]
  (ok> (nor/simplify-term t1) :as t1
       (nor/simplify-term t2) :as t2
       (if (and (first-order-term? t1) (first-order-term? t2))
         (first-order-unify t1 t2)
         (huet (list [t1 t2])))))
