(ns clj-lprolog.pcf.pcf
  (:require [clj-lprolog.core :as lp]))

;;{
;; The types and constants needed to encode the terms in a simple
;; functional programming language PCF
;;}

;; Terms
(lp/deftype tm)

(lp/defconst abs (-> (-> tm tm) tm)) ;; Abstraction
(lp/defconst app (-> tm tm tm)) ;; Application
(lp/defconst fixpt (-> (-> tm tm) tm)) ;; Fixpoint
(lp/defconst cond (-> tm tm tm tm)) ;; Conditional

;; Primitives
(lp/defconst ib (-> boolean tm)) ;; Embedding booleans into the language
(lp/defconst && tm)
(lp/defconst || tm)

(lp/defconst in (-> int tm)) ;; Embedding integers into the language
(lp/defconst minus tm)
(lp/defconst plus tm)
(lp/defconst times tm)
(lp/defconst equal tm)
(lp/defconst greater tm)
(lp/defconst zerop tm)

(lp/defconst ni tm)
(lp/defconst cs tm)
(lp/defconst car tm)
(lp/defconst cdr tm)
(lp/defconst nullp tm)
(lp/defconst consp tm)
