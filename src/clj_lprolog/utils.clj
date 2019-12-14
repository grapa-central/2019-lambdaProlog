(ns clj-lprolog.utils)

;; Taken from https://gitlab.com/fredokun/deputy/blob/master/src/deputy/utils.clj

;;; self-testing examples
;;; =====================

(def +examples-enabled+)

(defmacro example
  "Show as an example the evaluation of `expr` as `val`.
  Evaluate the example as a test an throw an exception if it fails,
  if a variable `+examples-enabled+` is set and bound to a truthy value."
  [expr sep val & {:keys [equiv?]
                   :or { equiv? =}}]
  (when (not= (name sep) "=>")
    (throw (ex-info "Missing '=>' in example" {:expr `(quote ~expr)
                                               :sep `(quote ~sep)
                                               :val `(quote ~val)})))
  (when-let [ex-var (find-var (symbol (str *ns*) "+examples-enabled+"))]
    (when (var-get ex-var)
      `(let [expr# ~expr
             val# ~val]
         (if (~equiv? expr# val#)
           val#
           (throw (ex-info "Example failed" {:expr ~`(quote ~expr)
                                             :val expr#
                                             :expected  ~`(quote ~val) })))))))

(defmacro examples
  "A variant of [[example]] with multiple clauses."
  [& clauses]
  (loop [clauses clauses, stmts []]
    (if (seq clauses)
      (if (< (count clauses) 3)
        (throw (ex-info "Wrong clause, not of the form `expr => val`"
                        {:clause clauses}))
        (let [[expr arr val & clauses'] clauses]
          (recur clauses' (conj stmts (list 'example expr arr val)))))
      ;; no more clause
      `(do ~@stmts))))


(defmacro do-for-example
  "A `do`-like construct only compiled if a
  variable `+examples-enabled+` is bound in the current namespace.
  This is used to prepare examples with some variable or function
  definitions."
  [& body]
  (when-let [ex-var (find-var (symbol (str *ns*) "+examples-enabled+"))]
    (when (var-get ex-var)
      `(do ~@body))))


;;; coll utilities
;;; ==============

(defn seq1?
  "Checks if `t` is a non-empty list."
  [t]
  (and (seq? t)
       (some? (seq t))))

(examples
 (seq1? ()) => false
 (seq1? '(a b)) => true
 (seq1? [1 2]) => false)

(defn vector1?
  "Checks if `t` is a non-empty vector."
  [t]
  (and (vector? t)
       (some? (seq t))))

;;; ok/ko monad
;;; ===========

(declare ok-expand)

(defmacro ok>
  "Chain forms evaluating to either a \"normal\" value
  or a *ko value* of the form `[:ko <msg> <info>]`.
  The first encountered *ko value* is returned, or chained with
  an explicit *ko form* following the last form evaluated to a
  *ko value*. In case no *ko value* is encountered, the value of
  the last non *ko form* is returned (an `:ok` value is often
  returned by default)

  This is akin to an *either* monad."

  [& stmts]
  (ok-expand stmts nil))

(defn ok-expr? [t]
  (or (= t :ok)
      (and (vector1? t)
           (= (first t) :ok))))

(defn ko-expr? [t]
  (or (#{:ko :ko>} t)
      (and (vector1? t)
           (#{:ko :ko>} (first t)))))

(defn chain-ko [ko1 ko2]
  (let [ko2' (if (and (ko-expr? ko1)
                      (vector? ko1))
               (assoc-in ko2 [2 :cause] ko1)
               ko2)]
    (if (vector? ko2')
      (into [:ko] (rest ko2'))
      ko2')))

(example (chain-ko [:ko "bla" {:bli :blu}]
                   [:ko "bloum" {:blam 42}])
         => [:ko "bloum" {:blam 42, :cause [:ko "bla" {:bli :blu}]}])

(defn mk-ok [ok-ex]
  (if (vector? ok-ex)
    (let [ok-ex (filterv #(not (nil? %)) ok-ex)]
      (if (> (count ok-ex) 1)
        ok-ex
        (first ok-ex)))
    ok-ex))

(example (mk-ok [:ok 1 2 3]) => [:ok 1 2 3])
(example (mk-ok [:ok 1 nil 4]) => [:ok 1 4])
(example (mk-ok [:ok]) => :ok)

(defn ok-expand [stmts last-res]
  (if (seq stmts)
    (cond
      ;; statement is a ko statement
      (ko-expr? (first stmts))
      (if (seq (rest stmts))
        (throw (ex-info "Garbage statements after :ko statement in `>ok` form."
                        {:ko-statment (first stmts)
                         :garbage (rest stmts)}))
        `~(first stmts))

      ;; statement is an ok statement
      (ok-expr? (first stmts))
      (if (seq (rest stmts))
        (throw (ex-info "Garbage statements after :ok statement in `>ok` form."
                        {:ok-statment (first stmts)
                         :garbage (rest stmts)}))
        `~(mk-ok (first stmts)))

      ;; not a ko nor an ok expression
      :else
      (let [res-sym (gensym "res")]
        `(let [~res-sym ~(first stmts)]
           (if (ko-expr? ~res-sym)
             ~(if (and (seq (rest stmts))
                       (ko-expr? (first (rest stmts))))
                `(chain-ko ~res-sym ~(first (rest stmts)))
                ;; :as then ko
                (if (and (seq (rest stmts))
                         (#{:as 'as} (first (rest stmts)))
                         (seq (drop 3 stmts))
                         (ko-expr? (first (drop 3 stmts))))
                  `(chain-ko ~res-sym ~(first (drop 3 stmts)))
                ;; next is not a ko form
                `~res-sym))
             ;; not a ko result
             ~(cond
                ;; skip ko form if any
                (and (seq (rest stmts))
                     (ko-expr? (first (rest stmts))))
                (ok-expand (drop 2 stmts) res-sym)

                ;; add a let binding if :as variant
                (and (seq (rest stmts))
                     (#{:as 'as} (first (rest stmts)))
                     (seq (drop 2 stmts)))
                (let [v (first (drop 2 stmts))]
                  #_(when (not (symbol? v))
                      ;; Note: too restrictive because of destructuring statements
                      (throw (ex-info "Expecting a variable after `:as` keyword in `>ok` form" {:expr v})))
                  `(let [~v ~res-sym]
                     ~(ok-expand (if (and (seq (drop 3 stmts))
                                          (ko-expr? (first (drop 3 stmts))))
                                   (drop 4 stmts)
                                   (drop 3 stmts)) res-sym)))

                :else
                (ok-expand (rest stmts) res-sym))))))

    ;; no more statement
    `~last-res))

(example
 (ok>) => nil)

(example
 (ok> :ok) => :ok)

(example
 (ok> [:ok 42]) => [:ok 42])

(example
 (ok> [:ko "blabla" {:bli :blu}])
 => [:ko "blabla" {:bli :blu}])

(example
 (ok> (identity 42)
      [:ko> "error" {:blu :bli}])
 => 42)

(example
 (ok> (identity 42)
      [:ko> "error" {:blu :bli}]
      (identity 54))
 => 54)

(example
 (ok> ((fn [] [:ko "bli" {:blu 42}])))
 => [:ko "bli" {:blu 42}])

(example
 (ok> (identity 42) ((fn [] [:ko "bli" {:blu 42}])) (identity 54))
 => [:ko "bli" {:blu 42}])

(example
 (ok> (identity 42)
      ((fn [] [:ko "bli" {:blu 42}]))
      [:ko> "bimbam" {:top true, :blim false}]
      (identity 54))
 => [:ko "bimbam" {:top true, :blim false, :cause [:ko "bli" {:blu 42}]}])

(examples
 (ok> (identity 42) :as x) => 42

 (ok> (identity 41) :as x
      (inc x)) => 42

 (ok> (identity 42) :as x
      [:ko> "boum" {:top 42}]
      (+ 54 x)) => 96

 (ok> (identity [:ko "bim" {:tip true}]) :as x
      [:ko> "boum" {:top 42}]
      (+ 54 x)) => '[:ko "boum" {:top 42, :cause [:ko "bim" {:tip true}]}])

(examples
 (ok> (vector (+ 40 2) (- 17 3)) :as [a b]
      (list a b)) => '(42 14)

 (ok> ((fn [] [:ko "boum" {}])) :as [a b]
      (list a b)) => [:ko "boum" {}]
 )

;;; More useful functions, by Basile Pesin

(defn ok-map
  "Like map, but taking oks and kos into account"
  [f l] (if (empty? l) [:ok l]
            (ok>
             (f (first l)) :as hd
             (ok-map f (rest l)) :as [_ tl]
             [:ok (cons (rest hd) tl)])))

(examples
 (ok-map (fn [x] [:ok x]) '(1 2 3)) => [:ok '([1] [2] [3])]
 (ok-map (fn [x] :ko) '(1 2 3)) => :ko
 (ok-map (fn [x] (if (> x 2) :ko [:ok x])) '(1 2 1)) => [:ok '([1] [2] [1])]
 (ok-map (fn [x] (if (> x 2) :ko [:ok x])) '(1 2 3)) => :ko)

(defn ok-reduce
  "Like reduce, but taking oks and kos into account"
  [f b l] (reduce (fn [x y] (ok> x :as [_ x] (f x y))) [:ok b] l))

(examples
 (ok-reduce (fn [x y] [:ok (+ x y)]) 0 '(1 2 3)) => [:ok 6]
 (ok-reduce (fn [x y] (if (< x y) [:ok y] :ko)) 0 '(1 2 3 4)) => [:ok 4]
 (ok-reduce (fn [x y] (if (< x y) [:ok y] :ko)) 0 '(1 2 1 4)) => :ko
 )

(defn every-ok?
  "Check if the predicates `p` returns an ok-expr for every element of `l`.
  If it doesn't, returns the first ko"
  [p l] (if (empty? l) :ok
            (ok> (p (first l))
                 (every-ok? p (rest l)))))

(examples
 (every-ok? (fn [x] :ok) '(1 2 3)) => :ok
 (every-ok? (fn [x] [:ko x]) '(1 2 3)) => [:ko 1]
 (every-ok? (fn [x] (if (= x 2) :ok [:ko x])) '(2 2 3 2)) => [:ko 3]
 )

;; Other utils

(defn map-of-pair-list
  "Turn a list of pairs into a map"
  [l] (reduce (fn [m [k v]] (assoc m k v)) {} l))
