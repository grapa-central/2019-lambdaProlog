(ns clj-lprolog.core
  "Provides the top-level functions for the latte-prolog interpreter"
  (:require [clj-lprolog.presyntax :as syn]))

;; Bind the syntax macros (defpred and addclause)
(defmacro defpred [& args] `(syn/defpred ~@args))
(defmacro addclause [& args] `(syn/addclause ~@args))
