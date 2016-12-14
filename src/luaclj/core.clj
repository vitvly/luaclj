(ns luaclj.core
  (:require [instaparse.core :as insta]
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

(comment
((lua if 3 < 5 then return "true" else return "false" end)) 
(lua
sum = 0
for j = 1 ","  99 do
  sum = sum + j
end

t = "{1, 10, 30}"
t"['a']" = 9
key = 22
t1 = "{a=2, b=3, c=4}"
t2 = "{[key]=10, [\"a\"]=20, [\"b\"]=30, [\"c\"]=40}"
t2.a = 30

for i","  v in ipairs(t2) do
  sum = sum + v
end
return sum
)
         )

