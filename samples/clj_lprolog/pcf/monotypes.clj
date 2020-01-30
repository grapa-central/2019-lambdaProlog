(ns clj-lprolog.pcf.monotypes
  (:require [clj-lprolog.core :as lp]))

;;{
;; Encoding of types, without polymorphism
;;}

(lp/deftype 'ty)

(lp/defconst '--> '(-> ty ty ty))
(lp/defconst 'lst '(-> ty ty))
(lp/defconst 'num 'ty)
(lp/defconst 'bool 'ty)
