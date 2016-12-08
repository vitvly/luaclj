(defproject luaclj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [instaparse "1.4.3"]
                 [riddley "0.1.12"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [com.rpl/specter "0.13.1"]]
  :java-source-paths ["src"]
  :aot [luaclj.BreakException])

