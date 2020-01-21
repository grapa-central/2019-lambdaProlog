(ns clj-lprolog.core
  "Provides the top-level functions for the latte-prolog interpreter"
  (:require [clj-lprolog.utils :as u]
            [clj-lprolog.presyntax :as syn]
            [clj-lprolog.typecheck :as typ]
            [clj-lprolog.solve :as sol]))

(defn start
  "Reset the program environment (useful for REPLS)"
  [] (syn/start))

;; Bind the syntax macros
(defmacro deftype [& args] `(syn/deftype ~@args))
(defmacro defconst [& args] `(syn/defconst ~@args))
(defmacro defpred [& args] `(syn/defpred ~@args))
(defmacro addclause [& args] `(syn/addclause ~@args))

(defn type-check-program
  "Type check the current program"
  [] (u/ok> (syn/verify-previous-declarations) :as _
            (typ/type-check-program
             (deref syn/progtypes)
             (deref syn/progconsts)
             (deref syn/progpreds))))

(defn solve
  "Solve the request `req`"
  [req] (u/ok>
         (syn/verify-previous-declarations) :as _
         (syn/parse-applied-pred req) :as [_ req]
         [:ko> 'parse-request {:req req}]
         (typ/elaborate-and-freevar-pred
          (deref syn/progconsts) (deref syn/progpreds) req) :as [_ req _]
         [:ko> 'typecheck-request {:req req}]
         (sol/solve (deref syn/progpreds) req)))
