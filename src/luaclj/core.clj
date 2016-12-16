(ns luaclj.core
  (:require [instaparse.core :as insta]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
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
            [luaclj.util :refer [name-map]]
            [luaclj.library :refer :all]
            [luaclj.parser :refer [parse-lua]]))

(defn lua->clj [lua-str & more]
  "Convert input Lua string into executable Clojure code.
   Additional parameters are:
    :fns - assume that lua-str contains a sequence of fn defs
    :nowrap - do not wrap Clojure code into an anonymous fn"
  (let [fns (contains? (set more) :fns)
        nowrap (contains? (set more) :nowrap)]
    (parse-lua lua-str (name-map fns nowrap))))

(defmacro eval-lua [lua-str & more]
  "Evaluate Lua code in current namespace"
  `(binding [~'*ns* ~*ns*]
    (eval (lua->clj ~lua-str ~@more))))

(defmacro lua [& code]
  "Clumsy lua macro. All commas, tables and table accessors should
  be surrounded in double quotes"
  (eval-lua
    (let [s (as-> code c
              (transform 
                (codewalker #(and (string? %1)
                                  (not= %1 ",")
                                  (not= (first %1) \{)
                                  (not= (first %1) \[)) ) 
                #(str "'" %1 "'") 
                c)
              (str/join " " c)
              )]
      ;(println "s:" s)
      s
      ) :nowrap))
