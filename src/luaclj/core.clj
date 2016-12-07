(ns luaclj.core
  (:require [instaparse.core :as insta]
            [clojure.string :as str]
            [luaclj.parser :refer [lua-parser
                                   transform-map]]))

(defn parse-lua [lua-str]
  (->> lua-str
       lua-parser
       (insta/transform transform-map)))

(defn get-lua-fn [lua-str]
  (eval (parse-lua lua-str)))

(defmacro create-lua-fn [lua-str]
  `(binding [~'*ns* ~*ns*]
    (get-lua-fn ~lua-str)))

(defmacro lua [& code]
  (create-lua-fn
    (let [s (as-> code c
              (map #(if (string? %1) (str "'" %1 "'") %1) c)
              (str/join " " c)
              (str/replace c "#{" "{")
              (str/replace c ":c" ","))]
      (println "s:" s)
      s
      )))
(comment
((lua if 3 < 5 then return "true" else return "false" end)) 
(lua
sum = 0
for j = 1 :c 99 do
  sum = sum + j
end

t = {1 :c 10 30}
t["a"] = 9
key = 22
t1 = #{a=2, b=3, c=4}
t2 = #{[key]=10, ["a"]=20, ["b"]=30, ["c"]=40}
t2.a = 30

for i :c v in ipairs(t2) do
  sum = sum + v
end
return sum
)
         )
