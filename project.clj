(defproject luaclj "0.1.0"
  :description "A Lua-to-Clojure parser"
  :url "https://github.com/siphiuel/luaclj"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [instaparse "1.4.3"]
                 [riddley "0.1.12"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [com.rpl/specter "0.13.1"]]
  :java-source-paths ["src"]
  :aot [luaclj.BreakException])

