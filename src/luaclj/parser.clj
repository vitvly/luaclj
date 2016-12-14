(ns luaclj.parser
  (:require [instaparse.core :as insta]
            [luaclj.proteus :refer [let-mutable]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.numeric-tower :as math]
            [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]
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

(def lua-parser (insta/parser (slurp-lua "resources/lua53.ebnf") :auto-whitespace :standard))


(defn find-global-vars [& args]
  (let [var-pred #(= (safe-some-> %1 first) 'set!)
        all-vars (set (map second
                           (select (codewalker var-pred) args)))
        local-pred #(= :local (safe-some-> %1 first))
        local-vars (set (map second
                             (select (codewalker local-pred) 
                                     args)))
        global-vars (set/difference all-vars local-vars)]
    (debug "global-vars:" all-vars ":" local-vars ":" global-vars)
    global-vars))

(defn chunk-fn [opts & args]
  (debug "chunk-fn:" args)
  (let [global-vars (find-global-vars args)
        global-var-init-statements (mapcat identity (map #(vector %1 nil) global-vars))
        args (transform 
               (codewalker #(= :local (safe-some-> %1 first))) 
               ; Remove occurrences of :local 
               second
               args)
        fn-body (if (and (not (:fns opts)) (seq global-var-init-statements))
                  `(let-mutable ~(into (vec global-var-init-statements) 
                                      `(~'_ ~@args)))
                  (first args))
        expr (if (or (:fns opts) (:nowrap opts))
               fn-body
               `(fn ~'anonymous-chunk [] ~fn-body))
        pred-fn #(= (safe-some-> %1 first) 'clojure.core/fn)
        edit-fn (fn [arg]
                  (let [fn-name (when (= (count arg) 4) (second arg))
                        fn-args (if (= (count arg) 4) (third arg)
                                  (second arg))
                        fn-body (if (= (count arg) 4) (fourth arg)
                                  (third arg)) 
                        fn-stmt (if fn-name
                                  `(fn ~fn-name ~fn-args (process-return ~fn-body))
                                  `(fn ~fn-args (process-return ~fn-body)))]
                    fn-stmt))
        expr (if (:nowrap opts)
               `(process-return ~(zipwalker expr pred-fn edit-fn))
               (zipwalker expr pred-fn edit-fn))]
  (debug "final chunk:" (macroexpand expr))
  (if (:fns opts)
    (map (fn [arg] `(def ~(second arg) ~(third arg)))
         (select [ALL (pred #(= (safe-some-> %1 first) 'set!))] expr))
    expr)))

(defn block-fn [& args]
  (debug "block-fn args:" args)
  (let [local-var-init-statements (select 
                                    [ALL #(= (safe-some-> %1 first first) :local)]
                                    args) 
        ;local-var-init-statements (get-var-init-statements :local args)
        wrap-with-return (fn [arg]
                           (debug "wrapping:" arg)
                           (with-meta arg {:stat true})
                           ;`(when (not :returned?) ~arg)
                           )
        transform-fn (fn [arg]
                       (cond (= :local (safe-some-> arg first first))
                             (mapcat identity (map #(list [:local (third %1)] (fourth %1)) arg))
                             :else `(~'_  ~(wrap-with-return arg))))
        r (cond (seq local-var-init-statements)
                `(let-mutable ~(vec (mapcat identity 
                                            (transform ALL transform-fn args))))
                (next args)
                `(do ~@(map wrap-with-return args))
                ;(= (safe-some-> args first first) 'clojure.core/fn)
                ;(first args)
                :else (wrap-with-return (first args)))]
    (debug "block-fn return:" r)
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
  (debug "string:" args)
  (.substring (first args) 1 (dec (count (first args)))))

(defn exp-fn [& args]
  (debug "exp-fn args:" args)
  (let [ r (cond (= 3 (count args))
    (list (nth args 1) (nth args 0) (nth args 2))
    (= 2 (count args))
    (list (nth args 1) (nth args 0))
    :else
    (first args))]
    (debug "exp-fn result:" r)
    r
    ))

(defn prefixexp-fn [& args]
  (if (next args)
    (second args)
    (first args)))

(defn varlist-fn [& args]
  (cons :varlist (leave :even args))
  )

(defn explist-fn [& args]
  (cons :explist (leave :even args))
  )

(defn var-fn [& args]
  ;(debug "var-fn args:" args)
  (cond (= (safe-some-> args second) "[")
    ; Table access
    `(get ~(first args) ~(nth args 2))
    (= (safe-some-> args second) ".")
    ; Table access through string keys
    `(get ~(first args) ~(str (nth args 2)))
    :else
    (first args)))

(defn get-while-statement [& args]
  (debug "while-args:" args)
  (let [while-args (leave :odd (first args))]
    (debug "while-args1:" while-args)
    `(process-break
       (while ~(first while-args)
       ~(second while-args)))
    ))
(defmacro repeat-until [test & body]
  `(process-break
     (loop []
       ~@body
       (when (not ~test)
         (recur)))))

(defn get-repeat-statement [& args]
  (debug "while-args:" args)
  (let [repeat-args (leave :odd (first args))]
    (debug "repeat-args1:" repeat-args)
    `(process-break
       (repeat-until ~(second repeat-args)
       ~(first repeat-args)))
    ))
(defn get-for-statement [& args]
  (debug "for-args:" args)
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
                                    (inc ~(nth for-args 2)) ; ranges are inclusive
                                    ~(nth for-args 3))
                            `(range ~(nth for-args 1) 
                                    (inc ~(nth for-args 2)))) ; ranges are inclusive
                          ]
                         [(vec (next (first for-args)))
                          (unwrap  (next (second for-args))
                                  #_(leave :even (next (second for-args))))
                          ]
                         )
        doseq-stmt `(process-break
                      (doseq ~for-condition
                        ~for-body))
        ]
    (debug "for-condition:" for-condition)
    (debug "for-args1:" for-args)
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
  (debug "get-fn-statement" args)
  (let [local? (= (first (first args)) "local")
        fn-def (if local? (next (first args))
                 (first args))
        fn-name (second fn-def)
        fn-args (vec (first (third fn-def)))
        fn-body (second (third fn-def))
        fn-signature `(fn ~fn-name ~fn-args ~fn-body)
        fn-stmt `(set! ~fn-name ~fn-signature)]
    (debug "fn-body:" (first (third fn-def)) ":" (second (third fn-def)))
    (if local? (conj fn-stmt :local) fn-stmt)))

(defn stat-fn [& args]
  (debug "stat-fn:" args)
  (let [var-set-fn (fn [local? name value]
                     ;(debug "var-set-fn:" name ":" value)
                     (let [set-stmt (if (= (safe-some-> name first) 'clojure.core/get)
                                      `(set! ~(second name)
                                             ~(apply list (conj (into ['assoc] (next name))
                                                                      #_(vec name) value)))
                                      `(set! ~name ~value))
                           set-stmt (if local? 
                                      (conj set-stmt :local) 
                                      set-stmt)]
                       set-stmt))
        r (cond (and (= (safe-some-> args first) "local")
                     (= (safe-some-> args second first) :namelist))
                (apply
                  (partial map (partial var-set-fn true))
                  (map next (leave :even (next args))))
              (or= "function" 
                   [(safe-some-> args first) 
                    (safe-some-> args second)])
              (get-fn-statement args)
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
              (= "do" (first args))
              (second args)
              (= "break" (first args))
              '(break)
              :else
              (first args))]
    (debug "stat-fn return:" r)
    ;`(:stat ~r)
    ;`(when (not :returned?) ~r)
    r
    ))

(defn binop-fn [arg1 op-str arg2]
  (let [op (case op-str
             "//" '(comp double floor /)
             "^" 'expt
             "%" 'mod
             "&" 'bit-and
             "~" 'bit-xor
             "|" 'bit-or
             ">>" 'bit-shift-right
             "<<" 'bit-shift-left
             "==" '=
             "~=" 'not=
             ".." 'str
             (symbol op-str))]
    (list op arg1 arg2)))

(defn unop-fn [op-str arg1]
  (let [op (case op-str
             "~" 'not
             "#" 'count
             (symbol op-str))]
    (list op arg1)))

(defn retstat-fn [& args]
  (debug "retstat args:" args)
  ;`(_ ~args)
  (let [ret-val (next (first (next args)))
        ret-val (do
                  (debug "ret-val:" ret-val)
                  (if (next ret-val) (vec ret-val)
                  (first ret-val)))]
        `(~'return ~ret-val)))

(defn namelist-fn [& args]
  (debug "namelist:" args)
  (into [:namelist] (leave :even args)))

(defn field-fn [& args]
  (debug "field-fn:" args)
  (let [r (cond (= (safe-some-> args first) "[")
                [(nth args 1) (nth args 4)]
                (= (count args) 3)
                [(str (first args)) (third args)]
                :else (first args))]
    (debug "field-fn r:" r)
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
    (debug "fieldlist:" r)
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
  (debug "functioncall:" args)
  (let [fn-args (if (or
                      (= (safe-some-> args second) [])
                      (= (safe-some-> args second first) :explist))
                  (or (-> args second next) [])
                  [(second args)])]
    `(~(first args) ~@fn-args)))

(defn funcname-fn [& args]
  (first args))

(defn funcbody-fn [& args]
  (debug "funcbody-fn in:" args)
  (let [r (remove #(or= %1 
                ["(" ")" "end"]) args)
        r (do
            (debug "funcbody:" r)
            (if (next r) r (conj r [])))]
    (debug "funcbody-fn result:" r)
    r
    ))

(defn parlist-fn [& args]
  (debug "parlist-fn:" args)
  (next (first args)))


(defn functiondef-fn [& args]
  (let [fn-args (vec (first (second args)))
        fn-body (second (second args))]
    `(fn ~fn-args ~fn-body)))

(defn tableconstructor-fn [& args]
  (debug "table:" args)
  (if (= 3 (count args))
    (into (hash-map) (second args))
    {}))


(def transform-map
  (into
    {:chunk chunk-fn
     :block block-fn
     :stat stat-fn
     :retstat retstat-fn
     :Numeral numeral-fn
     :LiteralString string-fn
     :Name symbol-fn
     :namelist namelist-fn
     :exp exp-fn
     :exp11 unop-fn
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
     :varlist varlist-fn }
    (map #(vector %1 binop-fn)
         [:exp1 :exp2 :exp3 :exp4 :exp5 :exp6
          :exp7 :exp8 :exp9 :exp10 :exp12])))


(defn parse-lua [lua-str opts]
  (->> lua-str
       lua-parser
       (insta/transform (assoc transform-map 
                               :chunk 
                               (partial chunk-fn opts)))))

(comment
(lua if i < 2 then 3 else 4 end dict = {2,3}
	"text"
	text = "more text" 
	654
  return dict[2])




     (try (anonymous-chunk)
     (catch Exception ex (clojure.stacktrace/print-stack-trace ex)))
     
  (pprint (lua-parser (slurp-lua "resources/test/function1.lua")))

(luaclj.util/process-return 
  (luaclj.proteus/let-mutable 
    [test1 nil a_fn nil b_fn nil test2 nil 
     _ (do 
         (set! test1 (clojure.core/fn test1 [] ^:stat (return 5))) 
         (set! test2 (clojure.core/fn test2 [] ^:stat (return 7))) 
         (do (set! a_fn (clojure.core/fn [] ^:stat (return (+ (test1) (test2)))))) 
         (do (set! b_fn (clojure.core/fn [arg] ^:stat (return (+ arg (+ (test1) (test2))))))) 
         ^:stat (return (+ (a_fn) (b_fn 2))))]))

(def test-fn (eval (insta/transform transform-map (lua-parser (slurp-lua "resources/test/function.lua")))))
((eval (parse-lua (slurp-lua "resources/test/function1.lua") {})))
((eval (insta/transform transform-map (lua-parser (slurp-lua "resources/test/function2.lua")))))
(try 
  ((eval (insta/transform transform-map (lua-parser (slurp-lua "resources/test/function1.lua")))))
       (catch Exception ex (clojure.stacktrace/print-stack-trace ex)))
(def fn1 (eval (insta/transform transform-map (lua-parser (slurp-lua "resources/test/basic1.lua")))))
(fn1)
((eval (parse-lua (slurp-lua "resources/test/convert_to.lua") {})))
((eval (insta/transform transform-map (lua-parser (slurp-lua "resources/test/tables.lua")))))
((eval (insta/transform transform-map (lua-parser (slurp-lua "resources/test/days_in_month.lua")))))
((eval (insta/transform transform-map (lua-parser (slurp-lua "resources/test/factorial.lua")))))
((eval (insta/transform transform-map (lua-parser (slurp-lua "resources/test/basic.lua")))))
((eval (insta/transform transform-map (lua-parser (slurp-lua "resources/test/basic1.lua")))))
((eval (insta/transform transform-map (lua-parser (slurp-lua "resources/test/for.lua")))))
((eval (insta/transform transform-map (lua-parser (slurp-lua "resources/test/break.lua")))))
((eval (insta/transform transform-map (lua-parser (slurp-lua "resources/test/precedence.lua")))))
((eval (insta/transform transform-map (lua-parser (slurp-lua "resources/test/precedence.lua")))))
(expt 3 2)
 (pprint (lua-parser (slurp-lua "resources/test/precedence.lua")))
[:chunk
 [:block
  [:stat
   "local"
   [:namelist [:Name "v"]]
   "="
   [:explist
    [:exp
     [:exp [:Numeral "3"]]
     [:binop "*"]
     [:exp [:exp [:Numeral "5"]] [:binop "+"] [:exp [:Numeral "3"]]]]]]
  [:retstat
   "return"
   [:explist [:exp [:prefixexp [:var [:Name "v"]]]]]]]]
(try (pprint (insta/transform transform-map (lua-parser "return 5*3 + 5")))
       (catch Exception ex (clojure.stacktrace/print-stack-trace ex)))
(luaclj.util/process-return (return (+ (* 5 3) 5)))
  
  
(try (pprint (insta/transform transform-map (lua-parser (slurp-lua "resources/test/precedence.lua"))))
       (catch Exception ex (clojure.stacktrace/print-stack-trace ex)))

  
  (pprint (insta/transform transform-map
                   (lua-parser "local sourcei, targeti = positions[sourceunits], positions[targetunits]")))


  (try (pprint (insta/transform transform-map (lua-parser (slurp-lua "resources/test/convert_to.lua"))))
       (catch Exception ex (clojure.stacktrace/print-stack-trace ex)))

  (try (pprint (insta/transform transform-map (lua-parser (slurp-lua "resources/test/break.lua"))))
       (catch Exception ex (clojure.stacktrace/print-stack-trace ex)))

  (try (pprint (insta/transform transform-map (lua-parser (slurp-lua "resources/test/factorial.lua"))))
       (catch Exception ex (clojure.stacktrace/print-stack-trace ex)))
 
  (try (pprint (insta/transform transform-map (lua-parser (slurp-lua "resources/test/days_in_month.lua"))))
       (catch Exception ex (clojure.stacktrace/print-stack-trace ex)))
 
  
  (try (pprint (insta/transform transform-map (lua-parser (slurp-lua "resources/test/function3.lua"))))
       (catch Exception ex (clojure.stacktrace/print-stack-trace ex)))
 
  (try (pprint (insta/transform transform-map (lua-parser (slurp-lua "resources/test/function2.lua"))))
       (catch Exception ex (clojure.stacktrace/print-stack-trace ex)))
  (try (pprint (insta/transform transform-map (lua-parser (slurp-lua "resources/test/tables.lua"))))
       (catch Exception ex (clojure.stacktrace/print-stack-trace ex)))
 
 (try (pprint (insta/transform transform-map (lua-parser (slurp-lua "resources/test/break.lua"))))
       (catch Exception ex (clojure.stacktrace/print-stack-trace ex)))
 (pprint (insta/transform transform-map (lua-parser (slurp-lua "resources/test/basic.lua"))))
  (try (pprint (insta/transform transform-map (lua-parser (slurp-lua "resources/test/basic1.lua"))))
       (catch Exception ex (clojure.stacktrace/print-stack-trace ex)))
  
  (try (pprint (insta/transform transform-map (lua-parser (slurp-lua "resources/test/for.lua"))))
       (catch Exception ex (clojure.stacktrace/print-stack-trace ex)))
  (pprint tree)
  (prewalk #(do (debug %1) %1) tree)
         )
