(ns clj-lprolog.core
  "Provides the top-level functions for the latte-prolog interpreter"
  (:require [clj-lprolog.presyntax :as syn]
            [clj-lprolog.typecheck :as typ]))

(defn start
  "Reset the program environment (useful for REPLS)"
  [] (swap! syn/progpreds (fn [_] {})))

;; Bind the syntax macros (defpred and addclause)
(defmacro defpred [& args] `(syn/defpred ~@args))
(defmacro addclause [& args] `(syn/addclause ~@args))

(defn type-check-program?
  "Type check the current program"
  [] (typ/type-check-program? (deref syn/progpreds)))
