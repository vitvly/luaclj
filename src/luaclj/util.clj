(ns luaclj.util
  (:import (luaclj.BreakException))
  (:require
    [riddley.walk :refer [walk-exprs]]
    [clojure.walk :as walk :refer [prewalk postwalk]]
    [luaclj.proteus :refer [let-mutable]]
    [clojure.zip :as zip]
    [clojure.string :as str]
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
(defmacro name-map [& v]
  (into {} 
        (map #(vector (keyword %1) %1) v)))

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


(defn end
  "returns the location loc where (end? (next loc)) is true."
  [loc]
  (loop [loc loc]
    (let [loc (zip/rightmost loc)]
      (if (zip/branch? loc)
        (recur (zip/down loc))
        loc))))

(defn skip
  "returns the next location that is not a child of this one"
  [start-loc]
  (loop [loc start-loc]
    (cond
      ; can't skip, jump to end
      (nil? loc) (zip/next (end start-loc))
      ; at end
      (zip/end? loc) loc
      ; go to right/up
      true (or (zip/right loc)
               (recur (zip/up loc))))))


(defn zipwalker [code pred-fn edit-fn & more]
  (zip/node 
    (let [skip-fn (first more)
          skip-fn (or skip-fn (constantly false))]
      (loop [c (zip/zipper sequential? seq (fn [_ c] c) code)
           cnt 0]
      (if (zip/end? c)
        c
        (let [curr-loc (if (pred-fn (zip/node c)) 
                         (zip/edit c edit-fn)
                         c)
              ;_ (println "curr-loc:" (zip/node curr-loc))
              next-loc (if (skip-fn (zip/node curr-loc))
                         (skip curr-loc)
                         (zip/next curr-loc))]
          ;(println "next-loc:" (zip/node next-loc))
          (recur next-loc (inc cnt))))))))
(defmacro process-return [code]
  ;(println "processed code in:" code)
  (let [r (if (and (sequential? code) 
           (= (count code) 2)
           (= (first code) 'return))
    (second code)
    (let [return-value (gensym)]
      (letfn [(pred-fn [arg]
                (and (sequential? arg) 
                     (:stat (meta arg))))
              (edit-fn [arg]
                ;(println "edit-fn:" arg)
                (if (= (first arg) 'return)
                  `(when-not ~return-value (set! ~return-value [~(second arg)]))
                  `(when-not ~return-value ~(vary-meta arg dissoc :stat)))
                )
              (skip-fn [arg]
                (= (safe-some-> arg first) 'luaclj.util/process-return))]
        `(let-mutable [~return-value nil]
           ~(zipwalker code pred-fn edit-fn skip-fn)
           (if ~return-value (first ~return-value) nil)))))]
    ;(println "processed code:" r)
    r
    ))

(defmacro process-break [code]
  `(try ~(zipwalker code 
                    #(= (safe-some-> %1 first) 'break) 
                    (fn [_] `(throw (luaclj.BreakException.))))
        (catch luaclj.BreakException ~'_ 1)))
#_(defmacro process-return [code]
  (transform 
    (codewalker (fn [arg]
                  (and (sequential? arg) (:stat (meta arg)))))
    #(if (= (first %1) 'return) 
       (second %1) %1)
    code))
#_(defmacro process-return [code]
  (println "process-return IN")
  (let [return-value (gensym)]
    (letfn [(predicate-fn  [arg]
              (and (sequential? arg) 
                   (not= (first arg) 'clojure.core/fn)
                   (:stat (meta arg))))
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
      (println "process-return OUT")
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

(defn slurp-lua [fname]
     (let [line-sep (System/getProperty "line.separator")
           file-contents (slurp fname)]
       (-> file-contents 
           (str/replace #"(?s)--\[\[.*\]\]" "")
           (str/replace #"(?m)--.*$" "")
           )))

(comment
  
(str/replace (str/join "\n" ["--[[ first comment" "code" "another comment]]"]) 
             #"(?sm)--\[\[.*\]\]"  "")
  (slurp-lua "resources/test/days_in_month.lua")
  )
