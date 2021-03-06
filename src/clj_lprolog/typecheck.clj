(ns clj-lprolog.typecheck
  "Typechecking of both lambda-terms and full program"
  (:require [clj-lprolog.utils :as u :refer [example examples ok>]]
            [clj-lprolog.syntax :as syn]
            [clojure.test :as t]
            [clojure.string :as str]))

(def +examples-enabled+ true)

;;{
;; Verification of declarations
;;}

(defn valid-type?
  "Check that `ty` is built from primitive and user-defined types"
  [types ty] (cond
               (syn/prop-type? ty) :ok
               (syn/string-type? ty) :ok
               (syn/int-type? ty) :ok
               (syn/boolean-type? ty) :ok
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

(example (valid-type? '#{bool} '(-> int bool)) => :ok)

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
               [:ok (conj (apply-subst-subst s2 s1) (apply-subst-subst s1 s2))]))

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
  '{ + (-> int int int) - (-> int int int) * (-> int int int)
    quot (-> int int int) mod (-> int int int)
    and (-> boolean boolean boolean) or (-> boolean boolean boolean)
    = (-> A A boolean) not= (-> A A boolean)
    zero? (-> int boolean)
    <= (-> int int boolean) < (-> int int boolean)
    >= (-> int int boolean) > (-> int int boolean)})

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

(defn type-of
  "Retrieve type information for an elaborated term `t`"
  [t] (cond
        (syn/string-lit? t) 'string
        (syn/int-lit? t) 'int
        (syn/boolean-lit? t) 'boolean
        :else (get (meta t) :ty)))

(defn set-type
  "Set the type information for a term `t`"
  [t ty] (cond
           (syn/string-lit? t) t
           (syn/int-lit? t) t
           (syn/boolean-lit? t) t
           :else (with-meta t {:ty ty})))

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
      [:ok {} ty (set-type t ty) cnt])

     ;; t is a free variable
     (syn/free? t) (let [ty (fresh-tvar cnt)]
                     [:ok {} ty (set-type t ty) (inc cnt)])

     ;; t is a string literal
     (syn/string-lit? t) [:ok {} 'string t cnt]

     ;; t is an integer literal
     (syn/int-lit? t) [:ok {} 'int t cnt]

     ;; t is a boolean literal
     (syn/boolean-lit? t) [:ok {} 'boolean t cnt]

     ;; t is a primitive
     (syn/primitive? t)
     (ok> (rename-type-vars cnt (get primitive-env t)) :as ty
          [:ok {} ty (set-type t ty) (inc cnt)])

     ;; t is a user constant
     (syn/user-const? t)
     (ok> (when (not (contains? consts t)) [:ko 'user-const {:const t}])
          (rename-type-vars cnt (get consts t)) :as ty
          [:ok {} ty (set-type t ty) (inc cnt)])

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
      [:ok si ty (set-type (list 'λ (second t) t') ty) cnt])

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
      [:ok si' ty (set-type (cons hd' tl') ty) (inc cnt)]))))

(defn infer-term
  "Infer the type of `t`"
  ([t] (infer-term {} t))
  ([consts t]
   (ok> (subst-infer-term consts t) :as [_ _ ty _]
        [:ok ty])))

(examples
 (infer-term '+) => [:ok '(-> int int int)]
 (infer-term '(λ 2 #{0})) => [:ok '(-> Ty0 Ty1 Ty1)]
 (infer-term '((λ 2 #{1}) (λ 1 #{0}))) => [:ok '(-> Ty1 Ty2 Ty2)]
 (infer-term '((λ 1 #{0}) 0)) => [:ok 'int])

(defn apply-subst-metadata
  "Apply a substitution `si` in the metadata of `t`"
  [si t]
  (cond
    (syn/lambda? t)
    (set-type (list 'λ (second t) (apply-subst-metadata si (nth t 2)))
      (syn/normalize-ty (apply-subst-ty si (type-of t))))
    (syn/application? t)
    (set-type (map (fn [t] (apply-subst-metadata si t)) t)
              (syn/normalize-ty (apply-subst-ty si (type-of t))))
    :else (set-type t (syn/normalize-ty (apply-subst-ty si (type-of t))))))

(defn elaborate-term
  "Elaborate a term with its type information"
  ([consts t] (ok> (elaborate-term consts t 0) :as [_ t cnt]
                   [:ok t]))
  ([consts t cnt]
   (ok> (subst-infer-term consts t cnt) :as [_ si _ t cnt]
        [:ok (apply-subst-metadata si t) cnt])))

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

;;{
;; Type checking and elaboration of a whole program
;;}

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
    [:ok (cons (set-type (first p) ty) (reverse tl)) cnt])))

(declare elaborate-clause-body)

(defn elaborate-goal
  "Elaborate a goal `g`"
  [types consts prog g cnt]
  (cond
    ;; The goal is a pi-abstraction
    (syn/pi? g)
    (ok>
     (second g) :as [c _ ty]
     (check-const types [c ty]) :as _
     (elaborate-clause-body
      types (assoc consts c ty) prog (nth g 2) cnt) :as [_ body]
     [:ok (list 'Π (second g) body) cnt])
    ;; The goal is an implication
    (syn/imp? g)
    (ok>
     (elaborate-pred consts prog (second g) cnt) :as [_ hd cnt]
     (elaborate-clause-body types consts prog (nth g 2) cnt) :as [_ tl cnt]
     [:ok (list '=> hd tl) cnt])
    ;; The goal is a print directive
    (syn/print? g)
    (ok>
     (elaborate-term consts (second g) cnt) :as [_ t cnt]
     [:ok (list 'print t) cnt])
    ;; The goal is a read directive
    (syn/read? g)
    (ok>
     (elaborate-term consts (second g) cnt) :as [_ t cnt]
     [:ok (list 'read t) cnt])
    ;; The goal is an applied predicate
    (syn/applied-pred? g) (elaborate-pred consts prog g cnt)
    ;; Either a non well-formed term, or we forgot to implement something :p
    :else [:ko 'elaborate-goal {:g g}]))

(defn elaborate-clause-body
  "Elaborate a clause body `b`"
  [types consts prog b cnt]
  (if (empty? b) [:ok '() cnt]
      (ok>
       (elaborate-goal types consts prog (first b) cnt) :as [_ hd cnt]
       (elaborate-clause-body types consts prog (rest b) cnt) :as [_ tl cnt]
       [:ok (cons hd tl) cnt])))

(defn elaborate-clause
  "Elaborate terms of a clause `c`"
  [types consts prog c]
  (ok> (elaborate-pred consts prog (first c) 0) :as [_ hd cnt]
       [:ko> 'elaborate-clause {:clause c}]
       (elaborate-clause-body types consts prog (second c) cnt) :as [_ tl _]
       [:ko> 'elaborate-clause {:clause c}]
       [:ok [hd tl]]))

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
   [:ko> 'check-freevar-pred {:pred t}]))

(defn elaborate-and-freevar-pred
  "Elaborate an applied predicate `t`, and get its freevar types,
   while checking everything"
  [consts prog t]
  (ok> (elaborate-pred consts prog t) :as [_ t]
       (check-freevar-pred t) :as [_ vars]
       [:ok t vars]))

(example
 (elaborate-and-freevar-pred '{succ (-> int int)} {'even ['(-> int o)]}
                             '(even (succ N)))
 => [:ok '(even (succ N)) {'N 'int}])

(declare check-freevar-clause-body)

(defn check-freevar-goal
  "Check and get freevar types for an elaborated goal `g`"
  [g fvt]
  (cond
    ;; The goal is a pi-abstraction
    (syn/pi? g) (check-freevar-clause-body (nth g 2) fvt)
    ;; The goal is an implication
    (syn/imp? g)
    (ok> (check-freevar-pred (second g)) :as [_ fvt2]
         (combine-env fvt fvt2) :as [_ fvt]
         (check-freevar-clause-body (nth g 2) fvt))
    ;; The goal is a print directive
    (syn/print? g)
    (ok> (get-freevar-types (second g)) :as [_ fvt2]
         (combine-env fvt fvt2))
    ;; The goal is a read directive
    (syn/read? g)
    (ok> (get-freevar-types (second g)) :as [_ fvt2]
         (combine-env fvt fvt2))
    ;; The goal is an applied predicate
    (syn/applied-pred? g)
    (ok> (check-freevar-pred g) :as [_ fvt2]
         (combine-env fvt fvt2))
    :else [:ko 'check-freevar-goal {:g g}]))

(defn check-freevar-clause-body
  "Check and get freevar types for an elaborated clause body `b`"
  [b fvt]
  (if (empty? b) [:ok fvt]
      (ok> (check-freevar-goal (first b) fvt) :as [_ fvt]
           (check-freevar-clause-body (rest b) fvt))))

(defn check-freevar-clause
  "Check and get freevar types for an elaborated clause `c`"
  [prog c]
  (ok> (check-freevar-pred (first c)) :as [_ fvt]
       [:ko> 'check-freevar-clause {:clause c}]
       (check-freevar-clause-body (second c) fvt)
       [:ko> 'check-freevar-clause {:clause c :fvt fvt}]))

(defn apply-freevar-types
  "Propagate unified freevar types into the term `t`"
  [vars t] (cond
             (syn/free? t)
             (if-let [ty (get vars t)] (set-type t (syn/normalize-ty ty)) t)
             (syn/lambda? t)
             (set-type
              (list 'λ (second t) (apply-freevar-types vars (nth t 2)))
              (type-of t))
             (syn/application? t)
             (set-type
              (map (fn [t] (apply-freevar-types vars t)) t)
              (type-of t))
             :else t))

(declare apply-freevar-types-clause-body)

(defn apply-freevar-types-goal
  "Propagate unified freevar types into the goal `g`"
  [vars g] (cond
             (syn/pi? g)
             (list 'Π (second g)
                   (apply-freevar-types-clause-body vars (nth g 2)))
             (syn/imp? g)
             (list '=> (apply-freevar-types vars (second g))
                   (apply-freevar-types-clause-body vars (nth g 2)))
             (syn/print? g)
             (list 'print (apply-freevar-types vars (second g)))
             (syn/read? g)
             (list 'read (apply-freevar-types vars (second g)))
             (syn/applied-pred? g) (apply-freevar-types vars g)))

(defn apply-freevar-types-clause-body
  "Propagate unified freevar types into the clause body `b`"
  [vars b] (map (fn [g] (apply-freevar-types-goal vars g)) b))

(defn apply-freevar-types-clause
  "Propagate unified freevar types into the clause `c`"
  [vars [hd body]] [(apply-freevar-types vars hd)
                    (apply-freevar-types-clause-body vars body)])

(defn elaborate-and-freevar-clause
  "Check and get freevar types for an elaborated clause `c`"
  [types consts prog c]
  (ok> (elaborate-clause types consts prog c) :as [_ c]
       (check-freevar-clause prog c) :as [_ vars]
       (apply-freevar-types-clause vars c) :as c
       [:ok c vars]))

(example
 (elaborate-and-freevar-clause {} '{succ (-> int int)} {'even ['(-> int o)]}
                               ['(even (succ (succ N))) '((even N))]) =>
 [:ok ['(even (succ (succ N))) '((even N))] {'N 'int}])

(defn elaborate-program
  "Elaborate the whole program `prog`"
  [types consts prog]
  (ok> (u/ok-map
        (fn [[pred [ty clauses]]]
          (ok> (u/ok-map (fn [c] (elaborate-clause types consts prog c))
                         clauses) :as [_ clauses]
               [:ok [pred [ty (map (fn [[t]] t) clauses)]]]))
        prog) :as [_ prog]
       [:ok (u/map-of-pair-list (map (fn [[c]] c) prog))]))

(defn elaborate-and-check-program
  "Check and elaborate `prog`, and check that the use of freevars is coherent"
  [types consts prog]
  (ok> (check-consts types consts) :as _
       (check-preds types prog) :as _
       (u/ok-map
        (fn [[pred [ty clauses]]]
          (ok> (u/ok-map (fn [c] (elaborate-and-freevar-clause types consts prog c))
                         clauses) :as [_ clauses]
               [:ok [pred [ty (map (fn [[t]] t) clauses)]]]))
        prog) :as [_ prog]
       [:ok (u/map-of-pair-list (map (fn [[c]] c) prog))]))

(defn type-check-program
  "Returns true if the program is correctly typed, false otherwise"
  [types consts prog]
  (ok> (elaborate-and-check-program types consts prog) :ok))
