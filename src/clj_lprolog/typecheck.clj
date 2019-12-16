(ns clj-lprolog.typecheck
  "Typechecking of both lambda-terms and full program"
  (:require [clj-lprolog.utils :as u :refer [example examples ok>]]
            [clj-lprolog.syntax :as syn]
            [clj-lprolog.presyntax :as psyn]
            [clojure.test :as t]))

(def +examples-enabled+ true)

;;{
;; STLC type inference
;; Uses the standard type inference algorithm, with first order unification
;; There is no let-binding in the language, we don't care about generalization
;;}

(defn fresh-tvar [count] (symbol (str "ty" count)))

(example
 (fresh-tvar 42) => 'ty42)

(defn n-fresh-tvar [count n]
  [(map first (take n (iterate (fn [[_ count]] [(fresh-tvar count) (inc count)])
                    [(fresh-tvar count) (inc count)])))
   (+ count n)])

(example
 (n-fresh-tvar 42 5) => ['(ty42 ty43 ty44 ty45 ty46) 47])

(defn type-unif-var?
  "Is `t` a unification variable ?"
  [t] (and (symbol? t) (> (count (name t)) 2) (= (subs (name t) 0 2) "ty")))

(defn get-unif-vars
  "Get the unification variables appearing inside `ty`"
  [ty] (cond
         (type-unif-var? ty) (list ty)
         (syn/arrow-type? ty)
         (flatten (map get-unif-vars (rest ty)))
         :else '()))

(defn substitute-ty
  "Substitute `t` to `var` in `ty`"
  [var t ty] (cond
               (type-unif-var? ty) (if (= ty var) t ty)
               (syn/type-var? ty) ty
               (syn/nat-type? ty) ty
               (syn/prop-type? ty) ty
               (syn/arrow-type? ty)
               (cons '-> (map (fn [ty] (substitute-ty var t ty)) (rest ty)))))

(defn apply-subst-ty
  "Apply a substitution `si` to a type `ty`"
  [si ty] (reduce (fn [ty [var t]] (substitute-ty var t ty)) ty si))

(defn subst-clash?
  "Check if there is a clash between `s1` and `s2`"
  [s1 s2] (some (fn [[x y1]]
                  (if (contains? s2 x)
                    (let [y2 (get s2 x)]
                      (and (not (= y1 y2))
                           (not (or (type-unif-var? y1)
                                    (type-unif-var? y2))))))) s1))

(defn compose-subst
  "Compose two substitutions `s1` and `s2`, after checking that they dont clash"
  [s1 s2] (ok> (when (subst-clash? s1 s2) [:ko> 'subst-clash {:s1 s1 :s2 s2}])
               [:ok (conj s1 s2)]))

(examples
 (compose-subst {'ty1 'i} {'ty2 'o}) =>
 [:ok {'ty1 'i 'ty2 'o}]

 (compose-subst {'ty1 'i} {'ty1 'o}) =>
 [:ko> 'subst-clash {:s1 {'ty1 'i} :s2 {'ty1 'o}}])

(defn mgu-ty
  "Finds the most general unifier for `ty1` and `ty2`"
  [ty1 ty2]
  ((fn aux [tys si]
    (if (empty? tys) [:ok si]
        (let [[ty1 ty2] (first tys)]
          (cond
            ;; The two types are the same
            (= ty1 ty2) (recur (rest tys) si)

            ;; ty1 is a unification variable
            (type-unif-var? ty1)
            (ok>
             (when (some #{ty1} (get-unif-vars ty2))
               [:ko 'occur-check {:ty1 ty1 :ty2 ty2}])
             (compose-subst si {ty1 ty2}) :as [_ si]
             (aux (map (fn [[x y]]
                         [(apply-subst-ty si x) (apply-subst-ty si y)])
                       (rest tys)) si))

            ;; ty2 is a unification variable
            (type-unif-var? ty2)
            (ok>
             (when (some #{ty2} (get-unif-vars ty1))
               [:ko 'occur-check {:ty1 ty2 :ty2 ty1}])
             (compose-subst si {ty2 ty1}) :as [_ si]
             (aux (map (fn [[x y]]
                         [(apply-subst-ty si x) (apply-subst-ty si y)])
                       (rest tys)) si))

            ;; ty1 and ty2 are arrow types
            (and (syn/arrow-type? ty1) (syn/arrow-type? ty2))
            (let [ty1 (syn/flatten-arrow ty1)
                  ty2 (syn/flatten-arrow ty2)
                  [ty1 ty2]
                  (cond
                    (< (count ty1) (count ty2))
                    [ty1 (syn/curry-arrow ty2 (- (count ty2) (count ty1)))]
                    (> (count ty1) (count ty2))
                    [(syn/curry-arrow ty1 (- (count ty1) (count ty2))) ty2]
                    :else [ty1 ty2])]
              (recur (concat (rest (map (fn [x y] [x y]) ty1 ty2)) (rest tys)) si))

            ;; Types are not unifiable
            :else [:ko 'not-unifiable {:ty1 ty1 :ty2 ty2}]
            )))
    ) (list [ty1 ty2]) {}))

(examples
 (mgu-ty 'i 'i) => [:ok {}]
 (mgu-ty 'i 'ty1) => [:ok {'ty1 'i}]
 (mgu-ty '(-> ty1 ty2) '(-> i o)) => [:ok {'ty1 'i 'ty2 'o}])

(def primitive-env
  "Types of primitives"
  { 'O 'i 'S '(-> i i) '+ '(-> i i i) '* '(-> i i i) })

(defn subst-infer-term
  "Infer the type of `t`.
   The call to the 2-parameters version returns a vector with:
   - The substitution needed to be applied
   - The type of the term
   - The term enriched with metadata about its type"
  ([t] (ok> (subst-infer-term t [] 0) :as [_ si ty t _]
            [:ok si ty t]))
  ([t env cnt]
   (cond
     ;; t is a bound variable
     (syn/bound? t)
     (ok>
      (when (>= t (count env)) [:ko> 'outside-env {:n t :env env}])
      [:ok {} (nth env t) t cnt])
     ;; t is a free variable
     (syn/free? t) (let [ty (fresh-tvar cnt)]
                     [:ok {} ty (with-meta t {:ty ty}) (inc cnt)])
     ;; t is a primitive
     (syn/primitive? t)
     (ok> [:ok {} (get primitive-env t) t cnt])

     ;; t is a lambda-abstraction
     (syn/lambda? t)
     (ok>
      (n-fresh-tvar cnt (second t)) :as [ty1 cnt]
      (subst-infer-term (nth t 2) (concat (reverse ty1) env) cnt)
      :as [_ si ty2 t' cnt]
      [:ko 'type-infer-abs {:t t}]
      (map (fn [ty] (apply-subst-ty si ty)) ty1) :as ty1
      (if (zero? (second t)) ty2
          (concat (if (seq? ty1)
                    (cons '-> ty1)
                    (list '-> ty1)) (list ty2))) :as ty
      [:ok si ty (with-meta (list 'λ (second t) t') {:ty ty}) cnt])

     ;; t is an application
     (syn/application? t)
     (ok>
      (subst-infer-term (first t) env cnt) :as [_ sihd thd hd' cnt]
      (u/ok-reduce (fn [[l cnt] t]
                     (ok> (subst-infer-term t env cnt) :as [_ si ty t cnt]
                          [:ok [(cons [si ty t] l) cnt]]))
                   ['() cnt] (rest t)) :as [_ [sittl cnt]]
      (u/ok-reduce (fn [si1 [si2 _ _ _]] (compose-subst si1 si2))
                   {} sittl) :as [_ sitl]
      (map (fn [[_ ty _ _]] ty) sittl) :as ttl
      (map (fn [[_ _ t _]] t) sittl) :as tl'
      (fresh-tvar cnt) :as ta
      (mgu-ty thd (concat (cons '-> ttl) (list ta))) :as [_ si]
      [:ko 'type-infer-app {:t t}]
      (apply-subst-ty si ta) :as ty
      (compose-subst sihd sitl) :as [_ sihdtl]
      [:ko 'type-infer-app {:t t}]
      (compose-subst si sihdtl) :as [_ si']
      [:ko 'type-infer-app {:t t}]
      [:ok si' ty (with-meta (cons hd' tl') {:ty ty}) (inc cnt)]
      ))))

(defn infer-term
  "Infer the type of `t`"
  [t] (ok>
       (subst-infer-term t) :as [_ _ ty _]
       [:ok ty]))

(examples
 (infer-term '+) => [:ok '(-> i i i)]
 (infer-term '(λ 2 0)) => [:ok '(-> ty0 ty1 ty1)]
 (infer-term '((λ 2 1) (λ 1 0))) => [:ok '(-> ty1 (-> ty2 ty2))]
 (infer-term '((λ 1 0) (S O))) => [:ok 'i])

(defn apply-subst-metadata
  "Apply a substitution `si` in the metadata of `t`"
  [si t]
  (cond
    (syn/bound? t) t
    (syn/free? t)
    (vary-meta t (fn [me] {:ty (apply-subst-ty si (get me :ty))}))
    (syn/primitive? t) t
    (syn/lambda? t)
    (with-meta (list 'λ (second t) (apply-subst-metadata si (nth t 2)))
      {:ty (apply-subst-ty si (get (meta t) :ty))})
    (syn/application? t)
    (with-meta (map (fn [t] (apply-subst-metadata si t)) t)
      {:ty (apply-subst-ty si (get (meta t) :ty))})
    ))

(defn elaborate-term
  "Elaborate a term with its type information"
  ([t] (ok> (subst-infer-term t) :as [_ si _ t]
            [:ok (apply-subst-metadata si t)])))

(defn check-and-elaborate-term
  "Check that `t` has type `ty`,
   and returns the elaborated version according to this type"
  [t ty] (ok> (subst-infer-term t) :as [_ si ty2 t]
              (mgu-ty ty ty2) :as [_ si2]
              [:ko 'check-term {:t t :ty ty}]
              (compose-subst si si2) :as [_ si]
              [:ok (apply-subst-metadata si t)]))

(defn apply-subst-env
  "Apply the type substitution `si` in the environment `e`"
  [si e] (reduce (fn [e [x ty]] (assoc e x (apply-subst-ty si ty))) {} e))

(defn combine-env
  "Combine two typing environments `e1` and `e2`"
  [e1 e2] (ok> (u/ok-reduce (fn [s1 [x ty1]]
                              (if (contains? e2 x)
                                (ok> (get e2 x) :as ty2
                                     (mgu-ty ty1 ty2) :as [_ s2]
                                     [:ko 'combine-env {:e1 e1 :e2 e2}]
                                     (compose-subst s1 s2))
                                [:ok s1]))
                            {} e1) :as [_ si]
               [:ok (conj (apply-subst-env si e1) (apply-subst-env si e2))]))

(examples
 (combine-env {'A 'i} {'B 'o}) => [:ok {'A 'i, 'B 'o}]
 (combine-env {'A 'i} {'A 'ty1}) => [:ok {'A 'i}]
 (u/ko-expr? (combine-env {'A 'i} {'A 'o})) => :ko)

(defn elaborate-pred
  "Elaborate terms of an applied predicate `t`.
   The head of the predicate must appear in the program `prog`"
  [p prog]
  (ok> (when (not (contains? prog (first p)))
         [:ko 'predicate-not-found {:pred (first p)}])
       (first (get prog (first p))) :as ty
       (syn/destruct-arrow ty (count (rest p))) :as [params res]
       (when (not= res 'o)
         [:ko 'wrong-ret-type-for-predicate {:pred (first p) :ret-ty res}])
       (u/ok-map (fn [[t ty]]
                   (check-and-elaborate-term t ty))
                 (zipmap (rest p) params)) :as [_ tl]
       [:ko> 'elaborate-pred {:pred p}]
       [:ok (cons (first p) (map (fn [[x]] x) tl))]))

(defn elaborate-clause
  "Elaborate terms of a clause `c`"
  [c prog] (ok> (elaborate-pred (first c) prog) :as [_ hd]
                [:ko> 'elaborate-clause {:c c}]
                (u/ok-map (fn [t] (elaborate-pred t prog)) (second c)) :as [_ tl]
                [:ko> 'elaborate-clause {:c c}]
                [:ok [hd (map (fn [[t]] t) tl)]]))

(defn get-freevar-types
  "Get the types of free variables in `t`"
  [t] (cond
        (syn/bound? t) [:ok {}]
        (syn/free? t) [:ok {t (get (meta t) :ty)}]
        (syn/primitive? t) [:ok {}]
        (syn/lambda? t) (get-freevar-types (nth t 2))
        (syn/application? t)
        (u/ok-reduce (fn [e1 t] (ok> (get-freevar-types t) :as [_ e2]
                                    (combine-env e1 e2))) {} t)))

(defn check-freevar-pred
  "Check and get freevar types for an elaborated applied predicate `t`.
   The head of the predicate must appear in the program `prog`"
  [t prog]
  (ok> (when (not (contains? prog (first t)))
         [:ko 'predicate-not-found {:pred (first t)}])
       (first (get prog (first t))) :as ty
       (syn/destruct-arrow ty (count (rest t))) :as [params res]
       (when (not= res 'o)
         [:ko 'wrong-ret-type-for-predicate {:pred (first t) :ret-ty res}])
       (u/ok-reduce (fn [e1 [t ty]] (ok> (get-freevar-types t) :as [_ e2]
                                        (combine-env e1 e2)))
        {} (zipmap (rest t) params))
       [:ko> 'check-freevar-pred {:p t}]
       ))

(defn elaborate-and-freevar-pred
  "Elaborate an applied predicate `t`, and get its freevar types,
   while checking everything"
  [t prog] (ok> (elaborate-pred t prog) :as [_ t]
                (check-freevar-pred t prog) :as [_ vars]
                [:ok t vars]))

(example
 (elaborate-and-freevar-pred '(even (S N)) {'even ['(-> i o)]}) =>
 [:ok '(even (S N)) {'N 'i}])

(defn check-freevar-clause
  "Check and get freevar types for an elaborated clause `c`"
  [c prog]
  (ok> (check-freevar-pred (first c) prog) :as [_ fvt]
       [:ko> 'check-freevar-clause {:c c}]
       (u/ok-reduce (fn [e1 t] (ok> (check-freevar-pred t prog) :as [_ e2]
                                   (combine-env e1 e2)))
                    fvt (second c))
       [:ko> 'check-freevar-clause {:c c}]
       ))

(defn elaborate-and-freevar-clause
  "Check and get freevar types for an elaborated clause `c`"
  [c prog] (ok> (elaborate-clause c prog) :as [_ c]
                (check-freevar-clause c prog) :as [_ vars]
                [:ok c vars]))

(example
 (elaborate-and-freevar-clause ['(even (S (S N))) '((even N))]
                               {'even ['(-> i o)]}) =>
 [:ok ['(even (S (S N))) '((even N))] {'N 'i}])

(defn elaborate-program
  "Elaborate the whole program `prog`"
  [prog]
   (ok> (u/ok-map
    (fn [[pred [ty clauses]]]
      (ok> (u/ok-map (fn [c] (elaborate-clause c prog)) clauses) :as [_ clauses]
           [:ok [pred [ty (u/map-of-pair-list (map (fn [[t]] t) clauses))]]]))
    prog) :as [_ prog]
        [:ok (u/map-of-pair-list (map (fn [[c]] c) prog))]))

(defn elaborate-and-check-program
  "Elaborate `prog`, and check that the use of freevars is coherent"
  [prog]
  (ok> (elaborate-program prog) :as [_ prog]
       (u/every-ok?
        (fn [[_ [_ clauses]]]
          (u/every-ok? (fn [c] (check-freevar-clause c prog))
                  clauses)) prog)
         [:ok prog]))

(defn type-check-program?
  "Returns true if the program is correctly typed, false otherwise"
  [prog] (ok> (elaborate-and-check-program prog)
              :ok))
