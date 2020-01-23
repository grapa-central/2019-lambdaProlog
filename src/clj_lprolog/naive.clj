(ns clj-lprolog.naive
  (:require [clj-lprolog.core :as core]
            [clj-lprolog.typecheck :as typ]
            [clj-lprolog.unif :as unif]
            [clh-lprolog.syntax :as syn]))

(defn all-unif-vars
  "Set of all unification variables in `pairs`"
  [pairs]
  (->> pairs
      (reduce #(into %1 (first %2) (second %2)) #{})
      (map unif/get-freevars)))

(defn head
  "the head of a term in normal form"
  [t] (first (nth t 2)))

(defn rigid?
  [t] (syn/free? (head t)))

(defn flexible?
  [t] (not (rigid? t)))

(defn flex-flex-pair?
  ([[t s]] (and (flexible? t) (flexible? s)))
  ([_] false))

(defn generate
  "Crée le terme λy1..yp. h (H1 ...) ... (Hr....) pour un type donné"
  [env target-type])

;; E |- X : Tp -> ... -> T1 -> A, A atomique
;; on pose E' = E + {yi : Ti,...}
;; on collecte h tel que:  E' |- h : Ur -> ... -> U1 -> A
;; on construit pour tout les h:
;;   Soit Hi : Tp -> ... -> T1 -> Ui variables libres,
;;        ui = (Hi #{p-1} ... #{0})
;;        t = (λ h (h ur ... u1))
;;   On génère une substitution [X t]



(defn generate-and-test
  "takes a list of pairs of terms, "
  [terms env]
  (cond
    (every? flex-flex-pair? terms) [] ; the empty substitution is `[]`
    :else nil))
