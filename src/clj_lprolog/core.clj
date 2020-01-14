(ns clj-lprolog.core
  "Provides the top-level functions for the latte-prolog interpreter"
  (:require [clj-lprolog.presyntax :as syn]
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
  [] (typ/type-check-program
      (deref syn/progtypes)
      (deref syn/progconsts)
      (deref syn/progpreds)))

(defn solve
  "Solve the request `req`"
  [req] (sol/solve (deref syn/progpreds) req))
