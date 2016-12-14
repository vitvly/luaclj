(ns luaclj.core-test
  (:require [clojure.test :refer :all]
            [luaclj.util :refer [slurp-lua]]
            [luaclj.library :refer :all]
            [luaclj.core :refer :all]))
(comment
  (parse-lua (slurp-lua "resources/test/basic.lua"))
  (parse-lua (slurp-lua "resources/test/function.lua"))
)
(deftest a-test
  (testing "Lua test scripts"
    (is (= ((eval-lua (slurp-lua "resources/test/basic.lua"))) 11))
    (is (= ((eval-lua (slurp-lua "resources/test/basic1.lua"))) "local_var_modified"))
    (is (= ((eval-lua (slurp-lua "resources/test/break.lua"))) 3))
    (is (= ((eval-lua (slurp-lua "resources/test/for.lua"))) 5060))
    (is (= ((eval-lua (slurp-lua "resources/test/function1.lua"))) 26))
    (is (= ((eval-lua (slurp-lua "resources/test/function2.lua"))) 5))
    (is (= ((eval-lua (slurp-lua "resources/test/function3.lua"))) 34))
    (is (= ((eval-lua (slurp-lua "resources/test/factorial.lua"))) 120))
    (is (= ((eval-lua (slurp-lua "resources/test/days_in_month.lua"))) 31))
    (is (= ((eval-lua (slurp-lua "resources/test/tables.lua"))) 2645))
    (is (= ((eval-lua (slurp-lua "resources/test/convert_to.lua"))) 86400))
    (is (= ((eval-lua (slurp-lua "resources/test/precedence.lua"))) -79))
    ))

(deftest fns-test
  (testing "fns option"
    (is (= (count (lua->clj (slurp-lua "resources/test/function1.lua") :fns)) 2))
    (is (= ((eval (first (lua->clj (slurp-lua "resources/test/function1.lua") :fns)))) 5))
    (is (= ((eval (second (lua->clj (slurp-lua "resources/test/function1.lua") :fns)))) 7))
    ))

(deftest nowrap-test
  (testing "nowrap option"
    (is (= (eval-lua (slurp-lua "resources/test/function1.lua") :nowrap) 26))
    ))

(deftest macro-test
  (testing "Lua macros"
    (is (= (lua if 3 < 5 then return "true" else return "false" end) "true"))))
