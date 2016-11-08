(ns luaclj.core
  (:require [instaparse.core :as insta]
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
