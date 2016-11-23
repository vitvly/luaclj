(ns luaclj.core-test
  (:require [clojure.test :refer :all]
            [luaclj.library :refer :all]
            [luaclj.core :refer :all]))
(comment
  (parse-lua (slurp "resources/test/basic.lua"))
  (parse-lua (slurp "resources/test/function.lua"))
)
(deftest a-test
  (testing "Lua test scripts"
    (is (= ((create-lua-fn (slurp "resources/test/basic.lua"))) 11))
    (is (= ((create-lua-fn (slurp "resources/test/basic1.lua"))) "local_var_modified"))
    (is (= ((create-lua-fn (slurp "resources/test/for.lua"))) 5060))
    (is (= ((create-lua-fn (slurp "resources/test/function1.lua"))) 26))
    
    ))
