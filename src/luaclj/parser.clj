(ns luaclj.parser
  (:require [instaparse.core :as insta]
            [proteus :refer [let-mutable]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.set :as set]
            [luaclj.library :refer :all]
            [luaclj.util :refer :all]
            [com.rpl.specter :refer [select 
                                     ALL
                                     LAST
                                     STAY
                                     select-first
                                     pred
                                     if-path
                                     cond-path
                                     multi-path
                                     subselect 
                                     transform 
                                     codewalker
                                     walker]]
            [clojure.walk :as walk :refer [prewalk postwalk]]))

(def lua-parser (insta/parser (slurp "resources/lua53.ebnf") :auto-whitespace :standard))

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


(defn post-process-blocks [& args]
  (println "post-process-blocks" args)
  (let [set-var-pred #(= 'set! (safe-some-> %1 first))
        all-vars (set (map second
                           (select (codewalker set-var-pred) args)))
        local-pred #(= :local (safe-some-> %1 first))
        local-vars (set (map #(second %1) 
                             (select (codewalker local-pred) 
                                     args)))
        global-vars (set/difference all-vars local-vars)
        _ (println "global-vars:" all-vars ":" local-vars ":" global-vars)
        args (transform (codewalker local-pred) ; Remove occurrences of :local 
                        second
                        ;(fn [arg] `(~(second (first arg)) ~(second arg))) 
                        args)
        global-var-init-statements (mapcat identity (map #(vector %1 nil) global-vars))
        nested-block `(~'_ ~@args)
        let-statements (into (vec global-var-init-statements) nested-block)
        expr `(process-return (let-mutable ~let-statements))]
    (println "post-process-blocks out:" let-statements)
    ;(println "chunk-fn3:" global-var-init-statements)
    expr
    ))

(comment

  (select
    [ALL
     (if-path (pred #(= (safe-some-> %1 first) 'clojure.core/fn))
             LAST
             STAY)]
              
    `((clojure.core/fn :a :b)
      (let-mutable [:a :b])))
  (select (multi-path STAY (walker #(= (safe-some-> %1 first) 'clojure.core/fn)))
          `((clojure.core/fn :a :b)
            (let-mutable [:a :b])))
  )
(defn chunk-fn [& args]
  (println "chunk-fn:" args)
  (let [fn-pred #(= (safe-some-> %1 first) 'clojure.core/fn)
        r (transform 
            [(codewalker fn-pred) LAST]
            #_[ALL 
               (if-path (pred fn-pred)
                        LAST
                        STAY) ]
            #(post-process-blocks %1)
            args)
        r (do
            (println "chunk-fn1:" r)
            (if (not (some fn-pred r)) 
            `(fn ~'anonymous-chunk [] ~(post-process-blocks (first r)))
             (transform [ALL] #(apply list %1) r)))]
    (println "chunk-fn result:" r)
    r))

(defn block-fn [& args]
  (println "block-fn args:" args)
  (let [local-var-init-statements (select 
                                    [ALL #(= (safe-some-> %1 first) :local)]
                                    args) 
        ;local-var-init-statements (get-var-init-statements :local args)
        wrap-with-return (fn [arg]
                           (println "wrapping:" arg)
                           (with-meta arg {:stat true})
                           ;`(when (not :returned?) ~arg)
                           )
        transform-fn (fn [arg]
                       (cond (= :local (safe-some-> arg first))
                         `([:local ~(third arg)] ~(fourth arg))
                         :else `(~'_  ~(wrap-with-return arg))))
        r (cond (seq local-var-init-statements)
            `(let-mutable ~(vec (mapcat identity 
                                        (transform ALL transform-fn args))))
            (next args)
            `(do ~@(map wrap-with-return args))
            (= (safe-some-> args first first) 'clojure.core/fn)
            (first args)
            :else (wrap-with-return (first args)))]
    (println "block-fn return:" r)
    ;`(when (not :returned?) ~r)
    r
    ))

(defn symbol-fn [& args]
   (read-string (first args)))

(defn numeral-fn [& args]
  (let [numeral (str/join args)]
    (if (str/includes? numeral ".")
      (Double/parseDouble numeral)
      (Integer/parseInt numeral))))

(defn string-fn [& args]
  (println "string:" args)
  (.substring (first args) 1 (dec (count (first args)))))

(defn exp-fn [& args]
  (println "exp-fn args:" args)
  (let [ r (cond (= 3 (count args))
    (list (nth args 1) (nth args 0) (nth args 2))
    (= 2 (count args))
    (list (nth args 1) (nth args 0))
    :else
    (first args))]
    (println "exp-fn result:" r)
    r
    ))

(defn prefixexp-fn [& args]
  (first args))

(defn varlist-fn [& args]
  (cons :varlist (leave :even args))
  )

(defn explist-fn [& args]
  (cons :explist (leave :even args))
  )

(defn var-fn [& args]
  ;(println "var-fn args:" args)
  (cond (= (safe-some-> args second) "[")
    ; Table access
    `(assoc ~(first args) ~(nth args 2))
    (= (safe-some-> args second) ".")
    ; Table access through string keys
    `(assoc ~(first args) ~(str (nth args 2)))
    :else
    (first args)))

(defn get-while-statement [& args]
  (println "while-args:" args)
  (let [while-args (leave :odd (first args))]
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
  (let [repeat-args (leave :odd (first args))]
    (println "repeat-args1:" repeat-args)
    `(repeat-until ~(second repeat-args)
       ~(first repeat-args))
    ))
(defn get-for-statement [& args]
  (println "for-args:" args)
  (let [for-args (leave :odd (first args))
        numeric? (not= (safe-some-> for-args first first) :namelist)
        for-body (if numeric?
                   (if (= (count for-args) 5)
                     (nth for-args 4)
                     (nth for-args 3))
                   (nth for-args 2))
        for-condition  (if numeric?
                         [(nth for-args 0) 
                        (if (= (count for-args) 5)
                           `(range ~(nth for-args 1) 
                                   ~(nth for-args 2)
                                   ~(nth for-args 3))
                           `(range ~(nth for-args 1) 
                                   ~(nth for-args 2)))]
                         [(vec (leave :even (next (first for-args))))
                          (unwrap (leave :even (next (second for-args))))
                          ]
                         )
        doseq-stmt `(doseq ~for-condition
                      ~for-body)
        ]
    (println "for-condition:" for-condition)
    (println "for-args1:" for-args)
    doseq-stmt
    ))

(defn get-if-statement [& args]
  (let [if-args (leave :odd (first args))
        if-args (mapcat identity
                        (map #(if (next %1) %1 [:else (first %1)])
                             (partition-all 2 if-args)))]
    (cons 'cond if-args)
    ))

(defn get-fn-statement [& args]
  (println "get-fn-statement" args)
  (let [fn-name (second (first args))
        fn-args (vec (first (third (first args))))
        fn-body (second (third (first args)))]
    (println "fn-body:" (first (third (first args))) ":" (second (third (first args))))
    `(fn ~fn-name ~fn-args ~fn-body)))

(defn stat-fn [& args]
  (println "stat-fn:" args)
  (let [var-set-fn (fn [local? name value]
                     (let [set-stmt (if (= (safe-some-> name first) 'clojure.core/assoc)
                                      `(set! ~(second name)
                                             ~(apply list (conj (vec name) value)))
                                      `(set! ~name ~value))
                           set-stmt (if local? 
                                      (conj set-stmt :local) 
                                      set-stmt)]
                       set-stmt))
        r (cond (= (safe-some-> args first) "local")
              (apply concat (apply
                (partial map (partial var-set-fn true))
                (map next (leave :even (next args)))))
              (= (safe-some-> args first first)
                 :varlist)
              `(do ~@(apply
                (partial map (partial var-set-fn false))
                (map next (leave :even args))))
              (= "while" (first args))
              (get-while-statement args)
              (= "repeat" (first args))
              (get-repeat-statement args)
              (= "for" (first args))
              (get-for-statement args)
              (= "if" (first args))
              (get-if-statement args)
              (= "function" (first args))
              (get-fn-statement args)
              (= "do" (first args))
              (second args)
              :else
              (first args))]
    (println "stat-fn return:" r)
    ;`(:stat ~r)
    ;`(when (not :returned?) ~r)
    r
    ))
(defn binop-or-unop-fn [& args]
  (symbol (first args)))
(defn retstat-fn [& args]
  (println "retstat args:" args)
  ;`(_ ~args)
  (let [ret-val (next (first (next args)))
        ret-val (do
                  (println "ret-val:" ret-val)
                  (if (next ret-val) (vec ret-val)
                  (first ret-val)))]
        `(~'return ~ret-val)))

(defn namelist-fn [& args]
  (println "namelist:" args)
  (into [:namelist] args))

(defn field-fn [& args]
  (println "field-fn:" args)
  (let [r (cond (= (safe-some-> args first) "[")
                [(nth args 1) (nth args 4)]
                (= (count args) 3)
                [(str (first args)) (third args)]
                :else (first args))]
    (println "field-fn r:" r)
    r))

(defn fieldsep-fn [& args]
  (first args))

(defn fieldlist-fn [& args]
  (let [r (reduce
            (fn [acc value]
              (let [index (first acc)
                    fields (second acc)
                    index (if (sequential? value) index (inc index))
                    value (if (sequential? value) value [index value])]

                [index (conj fields value)]))
            [0 []]
            (leave :even args))]
    (println "fieldlist:" r)
    (second r)))

(defn args-fn [& args]
  (case (first args)
    "(" (if (= (second args) ")") 
          []
          (second args)) ;explist
    ;"{" (if (= (second args) "}") 
          ;{}
          ;(second args)) ;tableconstructor
    (first args) ;LiteralString
    ))

(defn functioncall-fn [& args]
  (println "functioncall:" args)
  (let [fn-args (if (or
                      (= (safe-some-> args second) [])
                      (= (safe-some-> args second first) :explist))
                  (or (-> args second next) [])
                  [(second args)])]
    `(~(first args) ~@fn-args)))

(defn funcname-fn [& args]
  (first args))

(defn funcbody-fn [& args]
  (println "funcbody-fn in:" args)
  (let [r (remove #(or= %1 
                ["(" ")" "end"]) args)
        r (do
            (println "funcbody:" r)
            (if (next r) r (conj r [])))]
    (println "funcbody-fn result:" r)
    r
    ))

(defn parlist-fn [& args]
  (println "parlist-fn:" args)
  (leave :even (next (first args))))


(defn functiondef-fn [& args]
  (let [fn-args (vec (first (second args)))
        fn-body (second (second args))]
  `(fn ~fn-args ~fn-body)))

(defn tableconstructor-fn [& args]
  (println "table:" args)
  (if (= 3 (count args))
    (into (hash-map) (second args))
    {}))


(def transform-map
  {:chunk chunk-fn
   :block block-fn
   :stat stat-fn
   :retstat retstat-fn
   :Numeral numeral-fn
   :binop binop-or-unop-fn
   :unop binop-or-unop-fn
   :LiteralString string-fn
   :Name symbol-fn
   :namelist namelist-fn
   :exp exp-fn
   :prefixexp prefixexp-fn
   :var var-fn
   :field field-fn
   :fieldlist fieldlist-fn
   :tableconstructor tableconstructor-fn
   :args args-fn
   :functioncall functioncall-fn
   :funcname funcname-fn
   :funcbody funcbody-fn
   :functiondef functiondef-fn
   :parlist parlist-fn
   :explist explist-fn
   :varlist varlist-fn
   })

(comment
  [:chunk
   [:block
    [:stat
     "function"
     [:funcname [:Name "test1"]]
     [:funcbody
      "("
      ")"
      [:block [:retstat "return" [:explist [:exp [:Numeral "5"]]]]]
      "end"]]
    [:stat
     "function"
     [:funcname [:Name "test2"]]
     [:funcbody
      "("
      ")"
      [:block [:retstat "return" [:explist [:exp [:Numeral "7"]]]]]
      "end"]]
    [:stat
     [:varlist [:var [:Name "a_fn"]]]
     "="
     [:explist
      [:exp
       [:functiondef
        "function"
        [:funcbody
         "("
         ")"
         [:block
          [:retstat
           "return"
           [:explist
            [:exp
             [:exp
              [:prefixexp
               [:functioncall
                [:prefixexp [:var [:Name "test1"]]]
                [:args "(" ")"]]]]
             [:binop "+"]
             [:exp
              [:prefixexp
               [:functioncall
                [:prefixexp [:var [:Name "test2"]]]
                [:args "(" ")"]]]]]]]]
         "end"]]]]]
    [:stat
     [:varlist [:var [:Name "b_fn"]]]
     "="
     [:explist
      [:exp
       [:functiondef
        "function"
        [:funcbody
         "("
         [:parlist [:namelist [:Name "arg"]]]
         ")"
         [:block
          [:retstat
           "return"
           [:explist
            [:exp
             [:exp [:prefixexp [:var [:Name "arg"]]]]
             [:binop "+"]
             [:exp
              [:exp
               [:prefixexp
                [:functioncall
                 [:prefixexp [:var [:Name "test1"]]]
                 [:args "(" ")"]]]]
              [:binop "+"]
              [:exp
               [:prefixexp
                [:functioncall
                 [:prefixexp [:var [:Name "test2"]]]
                 [:args "(" ")"]]]]]]]]]
         "end"]]]]]
    [:stat
     [:functioncall
      [:prefixexp [:var [:Name "print"]]]
      [:args
       "("
       [:explist
        [:exp
         [:exp
          [:prefixexp
           [:functioncall
            [:prefixexp [:var [:Name "a_fn"]]]
            [:args "(" ")"]]]]
         [:binop "+"]
         [:exp
          [:prefixexp
           [:functioncall
            [:prefixexp [:var [:Name "b_fn"]]]
            [:args
             "("
             [:explist [:exp [:prefixexp [:var [:Name "arg"]]]]]
             ")"]]]]]]
       ")"]]]]]
 (pprint (lua-parser (slurp "resources/test/function1.lua")))
(luaclj.util/process-return 
  (proteus/let-mutable 
    [g nil 
     _ (do 
         (cond true 
               (proteus/let-mutable 
                 [l "local_var" 
                  _ (do (set! l "local_var_modified")) 
                  _ (return l)])) 
         (do (set! g "global_var")) (return g))]))

(luaclj.util/process-return
  (proteus/let-mutable
   [g
    nil
    _
    (do
     (cond
      true
      (proteus/let-mutable
       [l
        "local_var"
        _
        (do (set! l "local_var_modified"))
        _
        (return l)]))
     (do (set! g "global_var"))
     (return g))]))

(def test-fn (eval (insta/transform transform-map (lua-parser (slurp "resources/test/function.lua")))))
(eval (insta/transform transform-map (lua-parser (slurp "resources/test/function1.lua"))))
(def fn1 (eval (insta/transform transform-map (lua-parser (slurp "resources/test/basic1.lua")))))
(fn1)
((eval (insta/transform transform-map (lua-parser (slurp "resources/test/basic.lua")))))
((eval (insta/transform transform-map (lua-parser (slurp "resources/test/for.lua")))))
(eval (insta/transform transform-map (lua-parser (slurp "resources/test/break.lua"))))
  (try (pprint (insta/transform transform-map (lua-parser (slurp "resources/test/break.lua"))))
       (catch Exception ex (clojure.stacktrace/print-stack-trace ex)))
  
  
  (try (pprint (insta/transform transform-map (lua-parser (slurp "resources/test/function1.lua"))))
       (catch Exception ex (clojure.stacktrace/print-stack-trace ex)))
  (try (pprint (insta/transform transform-map (lua-parser (slurp "resources/test/basic1.lua"))))
       (catch Exception ex (clojure.stacktrace/print-stack-trace ex)))
  
  (try (pprint (insta/transform transform-map (lua-parser (slurp "resources/test/for.lua"))))
       (catch Exception ex (clojure.stacktrace/print-stack-trace ex)))
  (pprint tree)
  (prewalk #(do (println %1) %1) tree)
         )