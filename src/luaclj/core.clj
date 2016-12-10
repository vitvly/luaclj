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
            
            [luaclj.parser :refer [lua-parser
                                   transform-map]]))

(defn lua->clj [lua-str]
  (->> lua-str
       lua-parser
       (insta/transform transform-map)))

(defn get-lua-fn [lua-str]
  (eval (lua->clj lua-str)))

(defmacro create-lua-fn [lua-str]
  `(binding [~'*ns* ~*ns*]
    (get-lua-fn ~lua-str)))

(defmacro lua [& code]
  (create-lua-fn
    (let [s (as-> code c
              (transform 
                (codewalker #(and (string? %1)
                                    (not= %1 ",")
                                    (not= (first %1) \{)) ) 
                #(str "'" %1 "'") 
                c)
              (str/join " " c)
              )]
      (println "s:" s)
      s
      )))
(= (first "{sdfsf") \{)
(comment
((lua if 3 < 5 then return "true" else return "false" end)) 
(lua
sum = 0
for j = 1 ","  99 do
  sum = sum + j
end

t = "{1, 10, 30}"
t["a"] = 9
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
