(ns clj-lprolog.pcf.mono-test
  (:require [clj-lprolog.core :as lp]))

(load-file "samples/clj_lprolog/pcf/monoinfer.clj")
(load-file "samples/clj_lprolog/pcf/examples.clj")

(lp/defpred mono-test (-> string ty o))

(lp/addclause (mono-test String Type) :- (prog String Term) (infer Term Type))

(lp/solve (mono-test "successor" Ty))
(lp/solve (mono-test "onep" Ty))
(lp/solve (mono-test "issym" Ty))
(lp/solve (mono-test "fib" Ty))
(lp/solve (mono-test "fact" Ty))
(lp/solve (mono-test "map" Ty))
