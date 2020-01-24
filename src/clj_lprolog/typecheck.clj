(ns clj-lprolog.typecheck
  "Typechecking of both lambda-terms and full program"
  (:require [clj-lprolog.utils :as u :refer [example examples ok>]]
            [clj-lprolog.syntax :as syn]
            [clj-lprolog.presyntax :as psyn]
            [clojure.test :as t]
            [clojure.string :as str]))

(def +examples-enabled+ true)

;;{
;; STLC type inference
;; Uses the standard type inference algorithm, with first order unification
;; There is no let-binding in the language, we don't care about generalization
;;}

(defn fresh-tvar [count] (symbol (str "Ty" count)))

(example
 (fresh-tvar 42) => 'Ty42)

(defn n-fresh-tvar [count n]
  [(map first (take n (iterate (fn [[_ count]] [(fresh-tvar count) (inc count)])
                               [(fresh-tvar count) (inc count)])))
   (+ count n)])

(example
 (n-fresh-tvar 42 5) => ['(Ty42 Ty43 Ty44 Ty45 Ty46) 47])

(defn type-unif-var?
  "Is `t` a unification variable ?"
  [t] (and (symbol? t) (> (count (name t)) 2) (= (subs (name t) 0 2) "Ty")))

(defn get-unif-vars
  "Get the unification variables appearing inside `ty`"
  [ty] (cond
         (type-unif-var? ty) (list ty)
         (syn/applied-type-constructor? ty)
         (flatten (map get-unif-vars (rest ty)))
         :else '()))

(defn substitute-ty
  "Substitute `t` to `var` in `ty`"
  [var t ty] (cond
               (type-unif-var? ty) (if (= ty var) t ty)
               (syn/applied-type-constructor? ty)
               (cons (first ty)
                     (map (fn [ty] (substitute-ty var t ty)) (rest ty)))
               :else ty))

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

(defn apply-subst-subst
  "Apply `s1` to every value of `s2`"
  [s1 s2] (u/map-of-pair-list
           (map (fn [[k ty]] [k (apply-subst-ty s1 ty)]) s2)))

(defn compose-subst
  "Compose two substitutions `s1` and `s2`, after checking that they dont clash"
  [s1 s2] (ok> (when (subst-clash? s1 s2) [:ko> 'subst-clash {:s1 s1 :s2 s2}])
               [:ok (conj s1 (apply-subst-subst s1 s2))]))

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
         (let [[ty1 ty2] (first tys)
               ty1 (syn/normalize-ty (apply-subst-ty si ty1))
               ty2 (syn/normalize-ty (apply-subst-ty si ty2))]
           (cond
             ;; The two types are the same
             (= ty1 ty2) (recur (rest tys) si)

             ;; ty1 is a unification variable
             (type-unif-var? ty1)
             (ok>
              (when (some #{ty1} (get-unif-vars ty2))
                [:ko 'occur-check {:ty1 ty1 :ty2 ty2}])
              (compose-subst si {ty1 ty2}) :as [_ si]
              (aux (rest tys) si))

             ;; ty2 is a unification variable
             (type-unif-var? ty2)
             (ok>
              (when (some #{ty2} (get-unif-vars ty1))
                [:ko 'occur-check {:ty1 ty2 :ty2 ty1}])
              (compose-subst si {ty2 ty1}) :as [_ si]
              (aux (rest tys) si))

             ;; ty1 and ty2 are arrow types
             (and (syn/arrow-type? ty1) (syn/arrow-type? ty2))
             (let [n1 (dec (count ty1))
                   n2 (dec (count ty2))
                   [ty1 ty2]
                   (cond
                     (< n1 n2)
                     [ty1 (syn/curry-arrow ty2 (dec n1))]
                     (> n1 n2)
                     [(syn/curry-arrow ty1 (dec n2)) ty2]
                     :else [ty1 ty2])]
               (recur (concat (rest (map (fn [x y] [x y]) ty1 ty2))
                              (rest tys)) si))

             ;; ty1 and ty2 are applied type constructors
             (and (syn/applied-type-constructor? ty1)
                  (syn/applied-type-constructor? ty2)
                  (= (first ty1) (first ty1)) (= (count ty1) (count ty2)))
             (recur (concat (rest (map (fn [x y] [x y]) ty1 ty2))
                            (rest tys)) si)

             ;; Types are not unifiable
             :else [:ko 'not-unifiable {:ty1 ty1 :ty2 ty2}]
             )))
     ) (list [ty1 ty2]) {}))

(examples
 (mgu-ty 'i 'i) => [:ok {}]
 (mgu-ty 'i 'Ty1) => [:ok {'Ty1 'i}]
 (mgu-ty '(-> Ty1 Ty2) '(-> i o)) => [:ok {'Ty1 'i 'Ty2 'o}])

(def primitive-env
  "Types of primitives"
  { 'O 'i 'S '(-> i i) '+ '(-> i i i) '* '(-> i i i) })

(defn rename-type-vars
  "Change the type variables in `ty` into type-unification variables
  by prefixing \"Ty\" and suffixing `n` to their name"
  [n ty] (cond
           (syn/type-var? ty) (symbol (str "Ty" ty n))
           (syn/applied-type-constructor? ty)
           ;; An arrow type is actually of the same form
           (cons (first ty) (map (fn [ty] (rename-type-vars n ty)) (rest ty)))
           :else ty))

(example (rename-type-vars 42 '(-> A B A)) => '(-> TyA42 TyB42 TyA42))

(defn subst-infer-term
  "Infer the type of `t`.
   The call to the 2-parameters version returns a vector with:
   - The substitution needed to be applied
   - The type of the term
   - The term enriched with metadata about its type"
  ([t] (subst-infer-term {} t))
  ([consts t] (ok> (subst-infer-term consts t [] 0) :as [_ si ty t _]
                   [:ok si ty t]))
  ([consts t cnt] (ok> (subst-infer-term consts t [] cnt) :as [_ si ty t cnt]
                       [:ok si ty t cnt]))
  ([consts t env cnt]
   (cond
     ;; t is a bound variable
     (syn/bound? t)
     (ok>
      (when (>= (first t) (count env))
        [:ko> 'outside-env {:n (first t) :env env}])
      (nth env (first t)) :as ty
      [:ok {} ty (with-meta t {:ty ty}) cnt])

     ;; t is a free variable
     (syn/free? t) (let [ty (fresh-tvar cnt)]
                     [:ok {} ty (with-meta t {:ty ty}) (inc cnt)])

     ;; t is a primitive
     (syn/primitive? t)
     (ok> (get primitive-env t) :as ty
          [:ok {} ty (with-meta t {:ty ty}) cnt])

     ;; t is a user constant
     (syn/user-const? t)
     (ok> (when (not (contains? consts t)) [:ko 'user-const {:const t}])
          (rename-type-vars cnt (get consts t)) :as ty
          [:ok {} ty (with-meta t {:ty ty}) (inc cnt)])

     ;; t is a lambda-abstraction
     (syn/lambda? t)
     (ok>
      (n-fresh-tvar cnt (second t)) :as [ty1 cnt]
      (subst-infer-term consts (nth t 2) (concat (reverse ty1) env) cnt)
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
      (subst-infer-term consts (first t) env cnt) :as [_ sihd thd hd' cnt]
      (u/ok-reduce (fn [[l cnt] t]
                     (ok> (subst-infer-term consts t env cnt) :as [_ si ty t cnt]
                          [:ok [(cons [si ty t] l) cnt]]))
                   ['() cnt] (rest t)) :as [_ [sittl cnt]]
      (reverse sittl) :as sittl
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
      [:ok si' ty (with-meta (cons hd' tl') {:ty ty}) (inc cnt)]))))

(defn infer-term
  "Infer the type of `t`"
  ([t] (infer-term {} t))
  ([consts t]
   (ok> (subst-infer-term consts t) :as [_ _ ty _]
        [:ok ty])))

(examples
 (infer-term '+) => [:ok '(-> i i i)]
 (infer-term '(λ 2 #{0})) => [:ok '(-> Ty0 Ty1 Ty1)]
 (infer-term '((λ 2 #{1}) (λ 1 #{0}))) => [:ok '(-> Ty1 Ty2 Ty2)]
 (infer-term '((λ 1 #{0}) (S O))) => [:ok 'i])

(defn apply-subst-metadata
  "Apply a substitution `si` in the metadata of `t`"
  [si t]
  (cond
    (syn/lambda? t)
    (with-meta (list 'λ (second t) (apply-subst-metadata si (nth t 2)))
      {:ty (syn/normalize-ty (apply-subst-ty si (get (meta t) :ty)))})
    (syn/application? t)
    (with-meta (map (fn [t] (apply-subst-metadata si t)) t)
      {:ty (syn/normalize-ty (apply-subst-ty si (get (meta t) :ty)))})
    :else (vary-meta t (fn [me] {:ty (syn/normalize-ty
                                     (apply-subst-ty si (get me :ty)))}))
    ))

(defn elaborate-term
  "Elaborate a term with its type information"
  ([consts t]
   (ok> (subst-infer-term consts t) :as [_ si _ t]
        [:ok (apply-subst-metadata si t)])))

(defn check-and-elaborate-term
  "Check that `t` has type `ty`,
   and returns the elaborated version according to this type"
  ([consts t ty]
   (ok> (check-and-elaborate-term consts t ty 0) :as [_ t _]
        [:ok t]))
  ([consts t ty cnt]
   (ok> (subst-infer-term consts t cnt) :as [_ si ty2 t cnt]
        (mgu-ty ty ty2) :as [_ si2]
        [:ko 'check-term {:t t :ty ty}]
        (compose-subst si si2) :as [_ si]
        [:ok (apply-subst-metadata si t) cnt])))

(defn type-of
  "Retrieve type information for an elaborated term `t`"
  [t] (get (meta t) :ty))

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
 (combine-env {'A 'i} {'A 'Ty1}) => [:ok {'A 'i}]
 (u/ko-expr? (combine-env {'A 'i} {'A 'o})) => :ko)

(defn elaborate-pred
  "Elaborate terms of an applied predicate `t`.
   The head of the predicate must appear in the program `prog`"
  ([consts prog p] (ok> (elaborate-pred consts prog p 0) :as [_ p _]
                        [:ok p]))
  ([consts prog p cnt]
   (ok>
    (cond
      ;; The head is a user-defined predicate
      (syn/user-const? (first p))
      [(if (contains? prog (first p))
         (rename-type-vars cnt (first (get prog (first p))))
         [:ko 'predicate-not-found {:pred (first p)}]) (inc cnt)]
      ;; The head is a free variable
      (syn/free? (first p))
      [(cons '-> (concat
                  (first (n-fresh-tvar cnt (count (rest p))))
                  '(o))) (+ cnt (count p))]
      :else [:ko 'invalid-pred {:pred (first p)}]) :as [ty cnt]
    (syn/destruct-arrow ty (count (rest p))) :as [params res]
    (when (not= res 'o)
      [:ko 'wrong-ret-type-for-predicate {:pred (first p) :ret-ty res}])
    (u/ok-reduce
     (fn [[res cnt] [t ty]]
       (ok> (u/map-of-pair-list (map (fn [[x [ty _]]] [x ty]) prog)) :as prog
            (check-and-elaborate-term (conj prog consts) t ty cnt) :as [_ t cnt]
            [:ok [(cons t res) cnt]]))
     ['() cnt] (map vector (rest p) params)) :as [_ [tl cnt]]
    [:ko> 'elaborate-pred {:pred p}]
    [:ok (cons (with-meta (first p) {:ty ty}) (reverse tl)) cnt])))

(defn elaborate-clause
  "Elaborate terms of a clause `c`"
  [consts prog c]
  (ok> (elaborate-pred consts prog (first c) 0) :as [_ hd cnt]
       [:ko> 'elaborate-clause {:clause c}]
       (u/ok-reduce (fn [[res cnt] t]
                      (ok> (elaborate-pred consts prog t cnt) :as [_ t cnt]
                           [:ok [(cons t res) cnt]]))
                    ['() cnt] (second c)) :as [_ [tl cnt]]
       [:ko> 'elaborate-clause {:clause c}]
       [:ok [hd (reverse tl)]]))

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
  "Check and get freevar types for an elaborated applied predicate `t`"
  [t]
  (ok>
   (get (meta (first t)) :ty) :as ty
   (syn/destruct-arrow ty (count (rest t))) :as [params res]
   (when (not= res 'o)
     [:ko 'wrong-ret-type-for-predicate {:pred t :ret-ty res}])
   (u/ok-reduce (fn [e1 t] (ok> (get-freevar-types t) :as [_ e2]
                               (combine-env e1 e2)))
                (if (syn/free? (first t)) {(first t) ty} {})
                (rest t))
   [:ko> 'check-freevar-pred {:pred t}]
   ))

(defn elaborate-and-freevar-pred
  "Elaborate an applied predicate `t`, and get its freevar types,
   while checking everything"
  [consts prog t]
  (ok> (elaborate-pred consts prog t) :as [_ t]
       (check-freevar-pred t) :as [_ vars]
       [:ok t vars]))

(example
 (elaborate-and-freevar-pred {} {'even ['(-> i o)]} '(even (S N)))
 => [:ok '(even (S N)) {'N 'i}])

(defn check-freevar-clause
  "Check and get freevar types for an elaborated clause `c`"
  [prog c]
  (ok> (check-freevar-pred (first c)) :as [_ fvt]
       [:ko> 'check-freevar-clause {:clause c}]
       (u/ok-reduce (fn [e1 t] (ok> (check-freevar-pred t) :as [_ e2]
                                   (combine-env e1 e2)))
                    fvt (second c))
       [:ko> 'check-freevar-clause {:clause c :fvt fvt}]))

(defn elaborate-and-freevar-clause
  "Check and get freevar types for an elaborated clause `c`"
  [consts prog c] (ok> (elaborate-clause consts prog c) :as [_ c]
                       (check-freevar-clause prog c) :as [_ vars]
                       [:ok c vars]))

(example
 (elaborate-and-freevar-clause {} {'even ['(-> i o)]}
                               ['(even (S (S N))) '((even N))]) =>
 [:ok ['(even (S (S N))) '((even N))] {'N 'i}])

(defn elaborate-program
  "Elaborate the whole program `prog`"
  [consts prog]
  (ok> (u/ok-map
        (fn [[pred [ty clauses]]]
          (ok> (u/ok-map (fn [c] (elaborate-clause consts prog c))
                         clauses) :as [_ clauses]
               [:ok [pred [ty (map (fn [[t]] t) clauses)]]]))
        prog) :as [_ prog]
       [:ok (u/map-of-pair-list (map (fn [[c]] c) prog))]))

(defn valid-type?
  "Check that `ty` is built from primitive and user-defined types"
  [types ty] (cond
               (syn/nat-type? ty) :ok
               (syn/prop-type? ty) :ok
               (syn/user-type? ty)
               (if (contains? types ty) :ok [:ko 'check-type {:ty ty}])
               (syn/arrow-type? ty)
               (u/every-ok? (fn [ty] (valid-type? types ty)) (rest ty))
               (syn/applied-type-constructor? ty)
               (if (some (fn [t] (and (syn/applied-type-constructor? t)
                                     (= (first ty) (first t))
                                     (= (count ty) (count t)))) types)
                 (u/every-ok? (fn [ty] (valid-type? types ty)) (rest ty))
                 [:ko 'check-type {:ty ty}])
               ))

(example (valid-type? '#{bool} '(-> i bool)) => :ok)

(defn check-const
  "Check that the constant `c` use types declared in `types`"
  [types [c ty]] (ok> (valid-type? types ty) :as _
                      [:ko> 'check-const {:const c :ty ty}]))

(defn check-consts
  "Check that all `consts` use types declared in `types`"
  [types consts] (u/every-ok? (fn [c] (check-const types c)) consts))

(defn check-pred
  "Check that `p` uses types declared in `types`"
  [types [p [ty _]]] (ok> (valid-type? types ty) :as _
                          [:ko> 'check-pred {:pred p :ty ty}]
                          (when (not (syn/prop-type? (syn/return-type ty)))
                            [:ko 'check-pred {:pred p :ty ty}])
                          :ok))

(defn check-preds
  "Check that all preds in `prog` uses types declared in `types`"
  [types prog] (u/every-ok? (fn [p] (check-pred types p)) prog))

(defn elaborate-and-check-program
  "Check and elaborate `prog`, and check that the use of freevars is coherent"
  [types consts prog]
  (ok> (check-consts types consts) :as _
       (check-preds types prog) :as _
       (elaborate-program consts prog) :as [_ prog]
       (u/every-ok?
        (fn [[_ [_ clauses]]]
          (u/every-ok? (fn [c] (check-freevar-clause prog c))
                       clauses)) prog)
       [:ok prog]))

(defn type-check-program
  "Returns true if the program is correctly typed, false otherwise"
  [types consts prog]
  (ok> (elaborate-and-check-program types consts prog) :ok))
