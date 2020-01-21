(ns clj-lprolog.core
  "Provides the top-level functions for the latte-prolog interpreter"
  (:require [clj-lprolog.utils :as u :refer [ok>]]
            [clj-lprolog.presyntax :as syn]
            [clj-lprolog.typecheck :as typ]
            [clj-lprolog.solve :as sol]))

;; Contains the set of user types during execution of the program
(def progtypes (atom #{}))
;; Contains the set of constants (and their types) during execution of the program
(def progconsts (atom {}))
;; Contains the set of predicates during execution of the program
(def progpreds (atom {}))

(defn start
  "Reset the program environment"
  [] (do (swap! progtypes (fn [_] #{}))
         (swap! progconsts (fn [_] {}))
         (swap! progpreds (fn [_] {}))))

(defn verify-previous-declarations
  "Verify that the program defined by the previous declarations was correct"
  [] (ok> (deref progtypes) :as _
          (deref progconsts) :as _
          (deref progpreds) :as _))

;; Declaration macros, and the functions they use
(defn deftype-fun
  [ty] (ok> (verify-previous-declarations) :as _
            (if (syn/user-type-dec? ty)
              (swap! progtypes (fn [pt] (conj pt ty)))
              (swap! progtypes (fn [_] [:ko 'deftype {:ty ty}])))))

(defmacro deftype
  "Define a type. `n` is the name of the type, it should be lowercase"
  [ty] `(deftype-fun ~ty))

(defn defconst-fun
  [c ty] (ok> (verify-previous-declarations) :as _
              (if (syn/user-const-dec? c ty)
                (swap! progconsts
                       (fn [pc] (ok> (typ/check-const @progtypes [c ty])
                                    [:ko> 'defconst {:c c :ty ty}]
                                    (assoc pc c ty))))
                (swap! progconsts (fn [_] [:ko 'defconst {:c c :ty ty}])))))

(defmacro defconst
  "Define a constant `n` (should be lowercase),
  with its type `ty`. Checks well formedness"
  [c ty] `(defconst-fun ~c ~ty))

(defn defpred-fun
  [p ty] (ok> (verify-previous-declarations) :as _
              (if (and (syn/pred? p) (syn/proper-type? ty))
                (swap! progpreds
                       (fn [pp] (ok> (typ/check-pred @progtypes [p [ty '()]])
                                    [:ko> 'defpred {:p p :ty ty}]
                                    (assoc pp p [ty '()]))))
                (swap! progpreds (fn [_] [:ko 'defpred {:p p :ty ty}])))))

(defmacro defpred
  "Define a predicate. `n` is the name of the predicate, and `t` its type
  Also check well-formedness"
  [p ty] `(defpred-fun ~p ~ty))

(defn addclause-fun
  [clause]
  (ok> (verify-previous-declarations) :as _
       (swap! progpreds
              (fn [pp]
                (ok> (syn/parse-clause clause) :as [_ clause]
                     (get @progpreds (ffirst clause)) :as prev
                     (when (nil? prev) [:ko 'addclause {:clause clause}])
                     (typ/elaborate-and-freevar-clause
                      @progconsts @progpreds clause) :as [_ clause]
                     [:ko 'addclause {:clause clause}]
                     (assoc pp (first (first clause))
                            (list (first prev)
                                  (concat (second prev) (list clause))))
                     )))))

(defmacro addclause
  "Add `clause` to a predicate.
   Also checks that the clause is well formed"
  [clause] `(addclause-fun ~clause))

(defn type-check-program
  "Type check the current program"
  [] (u/ok> (verify-previous-declarations) :as _
            (typ/type-check-program
             (deref progtypes)
             (deref progconsts)
             (deref progpreds))))

(defn solve
  "Solve the request `req`"
  [req] (u/ok>
         (verify-previous-declarations) :as _
         (syn/parse-applied-pred req) :as [_ req]
         [:ko> 'parse-request {:req req}]
         (typ/elaborate-and-freevar-pred
          (deref progconsts) (deref progpreds) req) :as [_ req _]
         [:ko> 'typecheck-request {:req req}]
         (sol/solve (deref progpreds) req)))
