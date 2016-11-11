(ns luaclj.util
  (:require
    [riddley.walk :refer [walk-exprs]]
    [clojure.walk :as walk :refer [prewalk postwalk]]
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

(defmacro wrap-return [code]
  (
  code
  )
(comment
  (seq? `(some identity [1 2]))
  (def code '(do
               (if (< 3 5) 
                 (if (< 4 5)
                   (return "something")
                   (return "nested"))
                 (println "not something")) 
               [1 2 3]))
  (tree-seq code)
  (letfn [(predicate-fn [arg]
            (or= (safe-some-> arg first) ['if 'cond]))
          (handler-fn [arg]
            (println "arg:" arg)
            (do
                (println "if")
                `(when (not returned?) 
                   ~(transform [ALL] 
                               #(walk-exprs sequential? #_predicate-fn handler-fn %1) arg))))]
    (walk-exprs sequential? #_predicate-fn handler-fn code))
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
