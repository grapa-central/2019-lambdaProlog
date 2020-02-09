(ns clj-lprolog.pcf.examples
  (:require [clj-lprolog.core :as lp]))

(load-file "samples/clj_lprolog/pcf/pcf.clj")

(lp/defpred prog (-> string tm o))

;; Successor function
(lp/addclause (prog "successor"
                    (abs (λ [x] (app (app plus x) (in 1))))))

;; If the first argument is equal to 1, return the second, and the third otherwise
(lp/addclause
 (prog "onep"
       (abs (λ [w] (abs (λ [u] (abs (λ [v] (cond
                                             (app (app equal (in 1)) w)
                                             u v)))))))))

;; Check if f is symmetrical
(lp/addclause
 (prog "issym"
       (abs (λ [f] (abs (λ [x] (abs (λ [y] (app
                                            (app equal (app (app f x) y))
                                            (app (app f y) x))))))))))

;; Calculate fibonacci (in a very costly fashion)
(lp/addclause
 (prog "fib"
       (fixpt (λ [fib]
                 (abs (λ [n]
                         (cond (app (app greater (in 2)) n) n
                               (app (app plus
                                         (app fib (app (app minus n) (in 1))))
                                    (app fib (app (app minus n) (in 2))))
                               )))))))

;; Calculate the factorial, in a tail-recursive fashion
(lp/addclause
 (prog "fact"
       (fixpt
        (λ [f]
           (abs
            (λ [n]
               (abs
                (λ [m]
                   (cond (app (app equal n) (in 0)) m
                         (app (app f (app (app minus n) (in 1)))
                              (app (app times n) m)))))))))))

;; Good old map function
(lp/addclause
 (prog "map"
       (fixpt
        (λ [map]
           (abs
            (λ [f]
               (abs
                (λ [l]
                   (cond (app nullp l) ni
                         (app (app cs (app f (app car l)))
                              (app (app map f) (app cdr l))))))))))))
