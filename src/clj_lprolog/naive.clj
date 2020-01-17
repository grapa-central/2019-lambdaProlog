(ns clj-lprolog.naive
  (:require [clj-lprolog.core :as core]
            [clj-lprolog.typecheck :as typ]
            [clj-lprolog.unif :as unif]))

(defn all-unif-vars
  "Set of all unification variables in `pairs`"
  [pairs]
  (->> pairs
      (reduce #(into %1 (first %2) (second %2)) #{})
      (map unif/term-unknowns)))

(defn generate-and-test
  "takes a list of pairs of terms, "
  [terms env])
