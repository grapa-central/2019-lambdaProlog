(ns clj-lprolog.unif
  "Lambda term unification"
  (:require [clojure.set]
            [clj-lprolog.utils :as u :refer [example examples ok>]]
            [clj-lprolog.syntax :as syn]
            [clj-lprolog.typecheck :as typ]
            [clj-lprolog.norm :as nor]))

(def +examples-enabled+ true)

;;{
;; # Lambda term unification
;;
;; Based on Huet's algorithm
;; The unification variables are the free variables
;;}

(defn subst
  "Substitute a term `v` to the unification variable `var` in `t`"
  [var v t] (with-meta
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
              {:ty (typ/type-of t)}))

(example
 (subst 'A '(S O) '((λ 2 A) A B)) => '((λ 2 (S O)) (S O) B))

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

(defn term-unknowns
  "Get the set of unknown unification variables appearing in `t`"
  [t] (cond
        (syn/free? t) #{t}
        (syn/lambda? t) (term-unknowns (nth t 2))
        (syn/application? t)
        (reduce (fn [s t] (clojure.set/union s (term-unknowns t))) #{} t)
        :else #{}))

(example (term-unknowns '((λ 2 (A #{0})) A B (+ D C))) => #{'A 'B 'C 'D})

(defn trivial
  "Trivial unification : find the pairs containing a unification var
   and substitute them"
  [pairs]
  (if-let [[var t]
           (some
            #(cond
               (and (unif-var? (first %))
                    (not (contains? (term-unknowns (second %)) (head (first %)))))
               [(head (first %)) (second %)]
               (and (unif-var? (second %))
                    (not (contains? (term-unknowns (first %)) (head (second %)))))
               [(head (second %)) (first %)])
            pairs)]
    (trivial (filter
              (fn [[t1 t2]] (not= t1 t2))
              (map (fn [[t1 t2]] [(nor/normalize (subst var t t1))
                                 (nor/normalize (subst var t t2))])
                   pairs)))
    pairs))

(example (trivial '([(λ 0 (A)) (λ 0 (S))] [(λ 0 (B)) (λ 0 (A O))]
                    [(λ 2 (C)) (λ 2 (A B))]))
         => '([(λ 2 (C)) (λ 2 (S (S O)))]))

(defn normal-form-arg
  "Put an arg `e` into normal form with `n` abstractions,
  keep the right metadata from `ty`"
  [e n ty] (with-meta (nor/normalize (list 'λ n e))
             {:ty (cons '->
                        (concat (first (syn/destruct-arrow ty n))
                                (list (typ/type-of e))))}))

(example (normal-form-arg (with-meta 'O {:ty 'i}) 2 '(-> i i o))
         => '(λ 2 (O)))

(defn simpl
  "Simplification : among a number of pairs, apply the trivial procedure,
  then decompose the head normal forms and
  unify the parameters of rigide-rigide pairs"
  [pairs] (if (empty? pairs) [:ok pairs]
              (let [pairs (trivial (map (fn [[t1 t2]]
                                          [(nor/normalize t1) (nor/normalize t2)])
                                        pairs))]
                ;; If there is no rigid-rigid pair left, its over
                (if (every? (fn [[t1 t2]]
                              (or (flexible? t1) (flexible? t2))) pairs)
                  [:ok pairs]
                  (let [[t1 t2]
                        (some
                         (fn [[t1 t2]]
                           (when (and (rigid? t1) (rigid? t2))) [t1 t2])
                         pairs)
                        pairs (u/remove-first
                               (fn [[t1 t2]] (and (rigid? t1) (rigid? t2))) pairs)]
                    (if (or (not= (second t1) (second t2))
                            (not= (head t1) (head t2)))
                      [:ko 'not-unifiable {:t1 t1 :t2 t2}]
                      (simpl
                       (concat
                        pairs
                        (map
                         (fn [[e1 e2]]
                           [(normal-form-arg e1 (second t1) (typ/type-of t1))
                            (normal-form-arg e2 (second t2) (typ/type-of t2))])
                         (map vector (tail t1) (tail t2)))))))))))

(example (simpl '([(λ 0 (A)) (λ 0 (S))] [(λ 2 (A (B))) (λ 2 (A #{1}))])) =>
         '[:ok ([(λ 2 (B)) (λ 2 (#{1}))])])

(defn fresh-unknown [count] (symbol (str "H" count)))

(defn match
  "Non deterministic Match procedure : takes a flexible-rigid pair <`e1`, `e2`>,
  and return a set of possible candidates for unification,
  using imitation and projection"
  ([e1 e2] (first (match e1 e2 0)))
  ([e1 e2 cunknown]
  (let [v (head e1)
        vty (typ/type-of v)
        n (count (syn/param-types vty))
        ;; Imitation
        [imitation cunknown]
        (if (syn/primitive? (head e2))
          [#{[v
             (with-meta
               (list 'λ n
                     (with-meta
                       (cons (head e2)
                             (let [n (second e2)
                                   ty (typ/type-of (head e2))]
                               (map
                                (fn [[e i]] (with-meta
                                         (cons (with-meta
                                                 (fresh-unknown i)
                                                 {:ty (concat
                                                       (take (inc n) ty)
                                                       (list (typ/type-of e)))})
                                               (nor/eta-params ty))
                                         {:ty (typ/type-of e)}))
                                (map vector (tail e2)
                                     (take (count (tail e2))
                                           (iterate inc cunknown))))))
                       {:ty (typ/type-of (nth e2 2))}))
               {:ty (typ/type-of v)})]} (+ cunknown (count (tail e2)))]
          [#{} cunknown])
        ;; Projections
        [projections cunknown]
        (let [beta (syn/return-type vty)]
          (reduce
           (fn [[s cunknown] [i ty]]
             [(conj
               s [v
                  (with-meta
                    (list 'λ n
                          (with-meta
                            (cons (with-meta #{i} {:ty ty})
                                  (map (fn [[ty cunknown]]
                                         (with-meta
                                           (cons
                                            (with-meta
                                              (fresh-unknown cunknown)
                                              {:ty (concat
                                                    (take (inc n) vty)
                                                    (list ty))})
                                            (nor/eta-params vty))
                                           {:ty ty}))
                                       (map vector (syn/param-types ty)
                                            (take (count (syn/param-types ty))
                                                  (iterate inc cunknown)))))
                            {:ty beta}))
                    {:ty vty})]) (+ cunknown (count (syn/param-types ty)))])
           [#{} cunknown]
           (filter (fn [[i ty]] (= beta (syn/return-type ty)))
                   (map (fn [i] [i (typ/type-of (nth (reverse (tail e1)) i))])
                        (take (count (tail e1)) (iterate inc 0))))))]
    [(clojure.set/union imitation projections) cunknown])))
