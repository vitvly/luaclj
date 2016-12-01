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
    (is (= ((create-lua-fn (slurp-lua "resources/test/basic.lua"))) 11))
    (is (= ((create-lua-fn (slurp-lua "resources/test/basic1.lua"))) "local_var_modified"))
    (is (= ((create-lua-fn (slurp-lua "resources/test/break.lua"))) 3))
    (is (= ((create-lua-fn (slurp-lua "resources/test/for.lua"))) 5060))
    (is (= ((create-lua-fn (slurp-lua "resources/test/function1.lua"))) 26))
    (is (= ((create-lua-fn (slurp-lua "resources/test/function2.lua"))) 5))
    (is (= ((create-lua-fn (slurp-lua "resources/test/function3.lua"))) 34))
    (is (= ((create-lua-fn (slurp-lua "resources/test/factorial.lua"))) 120))
    (is (= ((create-lua-fn (slurp-lua "resources/test/days_in_month.lua"))) 31))
    
    ))
