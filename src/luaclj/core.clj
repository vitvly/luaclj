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
    (str/join " " 
              (map #(if (string? %1) (str "'" %1 "'") %1) 
                   code))))
(comment
((lua if 3 < 5 then return "true" else return "false" end)) 
         )
