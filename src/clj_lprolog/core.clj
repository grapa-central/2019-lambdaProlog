(ns clj-lprolog.core
  "Provides the top-level functions for the latte-prolog interpreter"
  (:require [clj-lprolog.utils :as u :refer [ok>]]
            [clj-lprolog.syntax :as syn]
            [clj-lprolog.presyntax :as psyn]
            [clj-lprolog.typecheck :as typ]
            [clj-lprolog.solve :as sol]
            [clj-lprolog.core :as lp]))

;; Contains the set of user types during execution of the program
(def progtypes (atom #{}))
;; Contains the set of constants (and their types) during execution of the program
(def progconsts (atom {}))
;; Contains the set of predicates during execution of the program
(def progpreds (atom {}))

;; Reset the program if needed
(defn start
  "Reset the program environment"
  [] (do (swap! progtypes (fn [_] #{}))
         (swap! progconsts (fn [_] {}))
         (swap! progpreds (fn [_] {}))))

;; Declaration macros, and the functions they use
(defn deftype-fun
  [ty] (if (psyn/user-type-dec? ty)
         (swap! progtypes (fn [pt] (conj pt ty)))
         (throw (ex-info (str 'deftype) {:ty ty}))))

(defmacro deftype
  "Define a type. `n` is the name of the type, it should be lowercase"
  [ty] `(deftype-fun '~ty))

(defn defconst-fun
  [c ty] (if (psyn/user-const-dec? c ty)
           (let [check (typ/check-const @progtypes [c ty])]
             (if (u/ko-expr? check)
               (throw (ex-info (str (second check)) (nth check 2)))
               (swap! progconsts (fn [pc] (assoc pc c ty)))))
           (throw (ex-info (str 'defconst) {:const c :ty ty}))))

(defmacro defconst
  "Define a constant `n` (should be lowercase),
  with its type `ty`. Checks well formedness"
  [c ty] `(defconst-fun '~c '~ty))

(defn defpred-fun
  [p ty] (if (and (syn/pred? p) (psyn/proper-type? ty))
           (let [check (typ/check-pred @progtypes [p [ty '()]])]
             (if (u/ko-expr? check)
               (throw (ex-info (str (second check)) (nth check 2)))
               (swap! progpreds (fn [pp] (assoc pp p [ty '()])))))
           (throw (ex-info (str 'defpred) {:pred p :ty ty}))))

(defmacro defpred
  "Define a predicate. `n` is the name of the predicate, and `t` its type
  Also check well-formedness"
  [p ty] `(defpred-fun '~p '~ty))

(defn addclause-fun
  [clause]
  (let [clause (psyn/parse-clause clause)]
    (if (u/ko-expr? clause)
      (throw (ex-info (str (second clause)) (nth clause 2)))
      (let [clause (typ/elaborate-and-freevar-clause
                    @progtypes @progconsts @progpreds (second clause))]
        (if (u/ko-expr? clause)
          (throw (ex-info (str (second clause)) (nth clause 2)))
          (swap! progpreds
                  (fn [pp]
                    (let [prev (get @progpreds (ffirst (second clause)))]
                      (assoc pp (first (first (second clause)))
                             (list (first prev)
                                   (concat (second prev) (list (second clause))))))
                     )))))))

(defmacro addclause
  "Add `clause` to a predicate.
   Also checks that the clause is well formed"
  [& clause] `(addclause-fun '~clause))

(defn type-check-program
  "Type check the current program"
  [] (typ/type-check-program
      (deref progtypes)
      (deref progconsts)
      (deref progpreds)))

(defn solve-fun
  "Actually solve the request `req`"
  [req] (u/ok>
         (psyn/parse-applied-pred req) :as [_ req]
         [:ko> 'parse-request {:req req}]
         (typ/elaborate-and-freevar-pred
          (deref progconsts) (deref progpreds) req) :as [_ req _]
         [:ko> 'typecheck-request {:req req}]
         (sol/solve (deref progconsts) (deref progpreds) req)))

(defmacro solve
  "Solve the request `req`"
  [req] `(lp/solve-fun '~req))
