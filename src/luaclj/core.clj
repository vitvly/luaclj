(ns luaclj.core
  (:require [instaparse.core :as insta]
            [proteus :refer [let-mutable]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.set :as set]
            [com.rpl.specter :refer [select 
                                     ALL
                                     select-first
                                     pred
                                     subselect 
                                     transform 
                                     walker]]
            [clojure.walk :as walk :refer [prewalk postwalk]]))

(def lua-parser (insta/parser (slurp "resources/lua53.ebnf") :auto-whitespace :standard))
(defmacro safe-some-> [& args]
  (cons 'some-> (interpose '((fn [arg] 
                              (when (sequential? arg) arg)))
                           args)))
(defmacro safe-some->> [& args]
  (cons 'some->> (interpose '((fn [arg] 
                              (when (sequential? arg) arg)))
                           args)))

(def third #(nth %1 2))
(def fourth #(nth %1 3))

(defn leave [odd-or-even-kw items]
  (map second (remove #((if (= odd-or-even-kw :odd) odd? even?) (first %1)) (map #(vector %1 %2) (range) items))))

(comment
  (let [s [[{:a 1} ['(set! a 3) :b #{'('(set! b 4))}]] ['(['(set! a 6)])]]
        walker-fn #(safe-some->> %1 (some (fn [arg] (= arg 'set!))))
        t-fn (fn [arg] (println "t-fn:" arg))]
    #_(select (walker walker-fn) s)
    (transform
      (subselect (walker walker-fn))
      t-fn
      s)
    )

  )
(defmacro or= [arg values]
  `(or ~@(map #(= arg %1) values)))

(defn chunk-fn [& args]
  (println "chunk-fn:" args)
  (let [set-var-pred #(= 'set! (safe-some-> %1 first))
        all-vars (set (map second
                           (select (walker set-var-pred) args)))
        local-pred #(= :local (safe-some-> %1 first))
        local-vars (set (map #(second %1) 
                             (select (walker local-pred) 
                                     args)))
        global-vars (set/difference all-vars local-vars)
        _ (println "global-vars:" all-vars ":" local-vars ":" global-vars)
        args (transform (walker local-pred) ; Remove occurrences of :local 
                        second
                        ;(fn [arg] `(~(second (first arg)) ~(second arg))) 
                        args)
        global-var-init-statements (mapcat identity (map #(vector %1 nil) global-vars))
        return-value (gensym)
        returned? (gensym)
        process-return-stmts (fn [arg]
                               (println "Process-return-stmts:" arg)
                               (let [ret-val (next (second arg))
                                     ret-val (if (next ret-val) 
                                               (vec ret-val) 
                                               (first ret-val))]
                                 `(do (set! ~returned? true)
                                      (set! ~return-value ~ret-val))))
        nested-block `(~'_ ~@args)
        let-statements (transform (walker #(= "return" (safe-some-> %1 first)))
                                  process-return-stmts
                                  nested-block)
        process-returned? (fn [arg]
                            returned?
                            )
        let-statements (transform (walker #(= %1 :returned?)) 
                                  process-returned? 
                                  let-statements)
        init-statements (vec (concat [return-value nil
                                     returned? false] 
                                    global-var-init-statements)) 
        let-statements (into init-statements let-statements)
        expr `(let-mutable ~let-statements ~return-value)]
    (println "chunk-fn2:" global-var-init-statements)
    expr
    ))

(defn block-fn [& args]
  (println "block-fn args:" args)
  (let [local-var-init-statements (select 
                                    [ALL #(= (safe-some-> %1 first) :local)]
                                    args) 
        ;local-var-init-statements (get-var-init-statements :local args)
        wrap-with-return (fn [arg]
                           (println "wrapping:" arg)
                           `(when (not :returned?) ~arg))
        transform-fn (fn [arg]
                       (cond (= :local (safe-some-> arg first))
                         `([:local ~(third arg)] ~(fourth arg))
                         :else `(~'_  ~(wrap-with-return arg))))
        r (cond (seq local-var-init-statements)
            `(let-mutable ~(vec (mapcat identity 
                                        (transform ALL transform-fn args))))
            (next args)
            `(do ~@(map wrap-with-return args))
            :else (wrap-with-return (first args)))]
    (println "block-fn return:" r)
    ;`(when (not :returned?) ~r)
    r
    ))

(defn symbol-fn [& args]
   (read-string (first args)))
(defn string-fn [& args]
  (println "string:" args)
  (.substring (first args) 1 (dec (count (first args)))))

(defn exp-fn [& args]
  (println "exp-fn args:" args)
  (cond (= 3 (count args))
    (list (nth args 1) (nth args 0) (nth args 2))
    (= 2 (count args))
    (list (nth args 1) (nth args 0))
    :else
    (first args)))

(defn prefixexp-fn [& args]
  (first args))

(defn varlist-fn [& args]
  (cons :varlist (leave :odd args))
  )

(defn explist-fn [& args]
  (cons :explist (leave :odd args))
  )

(defn var-fn [& args]
  (cond (or (contains? (set args) "[")
            (contains? (set args) "."))
    ; Table access
    (get (first args) (nth args 2))
    :else
    (first args)))

(defn get-while-statement [& args]
  (println "while-args:" args)
  (let [while-args (leave :even (first args))]
    (println "while-args1:" while-args)
    `(while ~(first while-args)
       ~(second while-args))
    ))
(defmacro repeat-until [test & body]
  `(loop []
     ~@body
     (when (not ~test)
       (recur))))

(defn get-repeat-statement [& args]
  (println "while-args:" args)
  (let [repeat-args (leave :even (first args))]
    (println "repeat-args1:" repeat-args)
    `(repeat-until ~(second repeat-args)
       ~(first repeat-args))
    ))
(defn get-for-statement [& args]
  (println "for-args:" args)
  (let [for-args (leave :even (first args))]
    (println "for-args1:" for-args)
    `(doseq [~(nth for-args 0) 
            (range ~(nth for-args 1) 
                   ~(nth for-args 2))]
      ~(nth for-args 3))
    ))

(defn get-if-statement [& args]
  (let [if-args (leave :even (first args))
        if-args (mapcat identity
                        (map #(if (next %1) %1 [:else (first %1)])
                             (partition-all 2 if-args)))]
    (cons 'cond if-args)
    ))
(defn stat-fn [& args]
  (println "stat-fn:" args)
  (let [r (cond (= (safe-some-> args first) "local")
              (apply concat (apply
                (partial map #(list :local 'set! %1 %2))
                (map next (leave :odd (next args)))))
              (= (safe-some-> args first first)
                 :varlist)
              `(do ~@(apply
                (partial map #(list 'set! %1 %2))
                (map next (leave :odd args))))
              (= "while" (first args))
              (get-while-statement args)
              (= "repeat" (first args))
              (get-repeat-statement args)
              (= "for" (first args))
              (get-for-statement args)
              (= "if" (first args))
              (get-if-statement args)
              :else
              args)]
    (println "stat-fn return:" r)
    ;`(:stat ~r)
    ;`(when (not :returned?) ~r)
    r
    ))
(defn binop-or-unop-fn [& args]
  (symbol (first args)))
(defn retstat-fn [& args]
  (println "restat args:" args)
  ;`(_ ~args)
  args
  )

(defn namelist-fn [& args]
  (println "namelist:" args)
  (into [:namelist] args))

(def transform-map
  {:chunk chunk-fn
   :block block-fn
   :stat stat-fn
   :retstat retstat-fn
   :Numeral symbol-fn
   :binop binop-or-unop-fn
   :unop binop-or-unop-fn
   :LiteralString string-fn
   :Name symbol-fn
   :namelist namelist-fn
   :exp exp-fn
   :prefixexp prefixexp-fn
   :var var-fn
   :explist explist-fn
   :varlist varlist-fn
   })

(comment
  (proteus/let-mutable
    [G__15153
     nil
     G__15154
     false
     i
     nil
     sum
     nil
     multi1
     nil
     multi2
     nil
     booleanValue
     nil
     _
     (do
       (clojure.core/when (clojure.core/not G__15154) (do (set! i 2)))
       (clojure.core/when
         (clojure.core/not G__15154)
         (do (set! booleanValue false)))
       (clojure.core/when
         (clojure.core/not G__15154)
         (do (set! multi1 1) (set! multi2 true)))
       (clojure.core/when
         (clojure.core/not G__15154)
         (clojure.core/while
           (< i 5)
           (clojure.core/when
             (clojure.core/not G__15154)
             (do (set! i (+ i 1))))))
       (clojure.core/when
         (clojure.core/not G__15154)
         ("repeat"
           (clojure.core/when
             (clojure.core/not G__15154)
             (do (set! i (+ i 1))))
           "until"
           (> i 10)))
       (clojure.core/when
         (clojure.core/not G__15154)
         (cond
           (> i 10)
           (clojure.core/when
             (clojure.core/not G__15154)
             (do (set! G__15154 true) (set! G__15153 "until")))
           (> i 3)
           (proteus/let-mutable
             [l
              "local_var"
              _
              (clojure.core/when
                (clojure.core/not G__15154)
                (do (set! G__15154 true) (set! G__15153 3)))])
           (== i 2)
           (clojure.core/when
             (clojure.core/not G__15154)
             (do (set! G__15154 true) (set! G__15153 44)))
           :else
           (clojure.core/when
             (clojure.core/not G__15154)
             (do (set! G__15154 true) (set! G__15153 55)))))
       (clojure.core/when (clojure.core/not G__15154) (do (set! sum 0)))
       (clojure.core/when
         (clojure.core/not G__15154)
         (clojure.core/doseq
           [j (clojure.core/range 1 100)]
           (clojure.core/when
             (clojure.core/not G__15154)
             (do (set! sum (+ sum j)))))))]
    G__15153)
(pprint (lua-parser (slurp "resources/test/basic.lua")))

(eval (insta/transform transform-map (lua-parser (slurp "resources/test/basic.lua"))))
  (try (pprint (insta/transform transform-map (lua-parser (slurp "resources/test/basic.lua"))))
       (catch Exception ex (clojure.stacktrace/print-stack-trace ex)))
  (pprint tree)
  (prewalk #(do (println %1) %1) tree)
         )
