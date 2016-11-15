(ns luaclj.util
  (:require
    [riddley.walk :refer [walk-exprs]]
    [clojure.walk :as walk :refer [prewalk postwalk]]
    [proteus :refer [let-mutable]]
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
    ))

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
(defn unwrap [coll]
  (if (next coll)
    coll
    (first coll)))

(defmacro or= [arg values]
  `(or ~@(map #(list = arg %1) values)))
(or= 1 [2 3 2])
(defn leave [odd-or-even-kw items]
  (map second 
       (remove #((if (= odd-or-even-kw :odd) even? odd?) (first %1)) 
               (map #(vector %1 %2) (range) items))))

(defmacro process-return [code]
  (let [return-value (gensym)]
    (letfn [(predicate-fn  [arg]
              (and (sequential? arg) (:stat (meta arg))))
            (walk-fn [arg]
              (println "handler" arg ":" (meta arg))
              (if (= (first arg) 'return)
                (do
                  (println "handler return:" (second arg))
                  `(when-not ~return-value (set! ~return-value [~(second arg)])))
                `(when-not ~return-value 
                   ~(transform [ALL]
                               #(transform (codewalker predicate-fn)
                                           walk-fn
                                           %1) 
                               arg))))]
      `(let-mutable [~return-value nil
         ~'_ ~(transform (codewalker predicate-fn)
        walk-fn
        code)]
         (if ~return-value (first ~return-value) nil)))))
#_(defmacro process-return [code]
  (let-mutable [return-value (gensym)]
    (println "process-return:" code)
    (letfn [(predicate-fn [arg]
              (:stat (meta arg)))
            (handler-fn [arg]
              (println "handler" arg ":" (meta arg))
              (if (:stat (meta arg))
                (if (= (first arg) 'return)
                  (do
                    (println "handler return:" (second arg))
                    `(when-not ~return-value (set! ~return-value [~(second arg)])))
                  `(when-not ~return-value 
                     ~(transform 
                        [ALL] 
                        #(walk-exprs sequential? handler-fn %1) arg)))
                (transform 
                  [ALL] 
                  #(walk-exprs sequential? handler-fn %1) arg)))]
      (let [r `(let-mutable [~return-value nil
                             ~'_ ~(walk-exprs sequential? handler-fn code)]
                 (if ~return-value (first ~return-value) nil))]
        (println "process-return result:" r)
        r))))

(comment
(proteus/let-mutable [G__13041 nil _ (do (clojure.core/when-not G__13041 (if (clojure.core/when-not G__13041 (. clojure.lang.Numbers (clojure.core/when-not G__13041 (clojure.core/lt 3 5)))) (clojure.core/when-not G__13041 (if (clojure.core/when-not G__13041 (. clojure.lang.Numbers (clojure.core/when-not G__13041 (clojure.core/lt 4 5)))) (set! G__13041 [(clojure.core/second luaclj.util/arg)]) (set! G__13041 [(clojure.core/second luaclj.util/arg)]))) (clojure.core/when-not G__13041 (println "not something")))) (clojure.core/when-not G__13041 [1 2 3]))] (if luaclj.util/return-value (clojure.core/first luaclj.util/return-value) nil))
(. clojure.lang.Numbers (clojure.core/lt 3 5))
(seq? `(some identity [1 2]))
(try (let [return1 (with-meta '(return "something") {:stat true})
           return2 (with-meta '(return "nested") {:stat true})]
       (process-return (do
                   (if (< 3 5) 
                     (if (< 4 5)
                       (return "something")
                       (return "nested"))
                     (println "not something")) 
                   [1 2 3])))
       (catch Exception ex (clojure.stacktrace/print-stack-trace ex)))
  (def code '(do
               (if (< 3 5) 
                 (if (< 4 5)
                   (return "something")
                   (return "nested"))
                 (println "not something")) 
               [1 2 3]))
  (process-return code)
  (walk-exprs (constantly true) identity code)
  (tree-seq code)
  (let-mutable [return-value (gensym)]
    (letfn [(predicate-fn [arg]
              (or= (safe-some-> arg first) ['if 'cond]))
            (handler-fn [arg]
              (println "arg:" arg)
              (if (= (first arg) 'return)
                `(set! ~return-value [(second arg)])
                `(when-not ~return-value 
                   ~(transform 
                      [ALL] 
                      #(walk-exprs sequential? handler-fn %1) arg))))]
      `(let-mutable [~return-value nil
                     ~'_ ~(transform [ALL] 
                                    #(walk-exprs sequential? handler-fn %1)
                                    code)]
         (if return-value (first return-value) nil))))
  (postwalk
    (fn [arg]

      (println "arg:" arg)
      (if (or= (safe-some-> arg first) ['if 'cond'])
        (do
          (println "if")
          `(when (not returned?) ~arg))
        arg))
    code)
  (try (letfn [(handler-fn  [arg]
                 (println "Handler:" arg) 
                 (cond (some #{'if 'cond} arg)
                   `(when (not returned?) ~arg)
                   (sequential? arg) 
                   (map handler-fn arg)
                   :else arg))]
         (walk-exprs (constantly true)
           handler-fn
           '(do
              (if (< 3 5) (return "something")) 
              [1 2 3])))
       (catch Exception ex (clojure.stacktrace/print-stack-trace ex)))
(transform (walker sequential?)
           (fn  [arg]
             (println "Handler:" arg) 
             (if (or= (first arg) ['if 'cond])
               `(when (not returned?) ~arg)
               arg))
           '(do
                   (if (< 3 5) (return "something")) 
                   [1 2 3]))
)
