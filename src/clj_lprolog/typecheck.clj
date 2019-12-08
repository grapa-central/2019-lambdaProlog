(ns clj-lprolog.typecheck
  "Typechecking of both lambda-terms and full program"
  (:require [clj-lprolog.syntax :as syn]
            [clj-lprolog.presyntax :as psyn]))

;;{
;; STLC type inference
;; Uses the standard type inference algorithm, with first order unification
;; There is no let-binding in the language, we don't care about generalization
;;}

(defn fresh-tvar [] (gensym "ty"))

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

(defn subst-clash
  "Check if there is a clash between `s1` and `s2`"
  [s1 s2] (some (fn [[x y1]]
                  (if (contains? s2 x)
                    (let [y2 (get s2 x)]
                      (and (not (= y1 y2))
                           (not (or (type-unif-var? y1)
                                    (type-unif-var? y2))))))) s1))

(defn compose-subst
  "Compose two substitutions `s1` and `s2`, after checking that they dont clash"
  [s1 s2] (if (subst-clash s1 s2) nil (conj s1 s2)))

(defn mgu-ty
  "Finds the most general unifier for `ty1` and `ty2`"
  [ty1 ty2]
  (loop [tys (list [ty1 ty2]) si {}]
    (if (empty? tys) si
        (let [[ty1 ty2] (first tys)]
          (cond
            (= ty1 ty2) (recur (rest tys) si)
            (type-unif-var? ty1)
            (if (some #{ty1} (get-unif-vars ty2)) nil
                (let [si (compose-subst si {ty1 ty2})]
                  (recur (map (fn [[x y]]
                                [(apply-subst-ty si x) (apply-subst-ty si y)])
                              (rest tys)) si)))
            (type-unif-var? ty2)
            (if (some #{ty2} (get-unif-vars ty1)) nil
                (let [si (compose-subst si {ty2 ty1})]
                  (recur (map (fn [[x y]]
                                [(apply-subst-ty si x) (apply-subst-ty si y)])
                              (rest tys)) si)))
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
              (recur (concat (map (fn [x y] [x y]) ty1 ty2) (rest tys)) si))
            :else nil ;; Types are not unifiable
      )))))

(def primitive-env
  "Types of primitives"
  { 'O 'i 'S '(-> i i) '+ '(-> i i i) '* '(-> i i i) })

(defn infer-term
  "Infer the type of `t`"
  ([t] (second (infer-term t [])))
  ([t env] (cond
             (syn/bound? t) [{} (nth env t)]
             (syn/free? t) [{} (fresh-tvar)]
             (syn/primitive? t) [{} (get primitive-env t)]
             (syn/lambda? t)
             (let [ ty1 (repeatedly (second t) fresh-tvar) ]
               (if-let [ res2 (infer-term (nth t 2)
                                          (concat (reverse ty1) env)) ]
                 (let [[si ty2] res2]
                   (if-let [ty1 (map (fn [ty] (apply-subst-ty si ty)) ty1)]
                     [si
                      (concat (if (seq? ty1)
                                (cons '-> ty1)
                                (list '-> ty1)) (list ty2))]))))
             (syn/application? t)
             (if-let [[sihd thd] (infer-term (first t) env)]
               (if-let [sittl (map (fn [t] (infer-term t env)) (rest t))]
                 (let [ttl (map (fn [[_ t]] t) sittl)
                       sitl (reduce (fn [si1 [si2 _]] (compose-subst si1 si2))
                                    {} sittl)
                       ta (fresh-tvar)]
                   (if-let [si (mgu-ty thd (concat (cons '-> ttl) (list ta)))]
                     [(compose-subst si (compose-subst sihd sitl))
                      (apply-subst-ty si ta)])))
               ))))
