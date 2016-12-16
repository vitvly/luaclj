# luaclj

This is a Lua-to-Clojure parser. It takes Lua source code string as an input and produces eval'able Clojure code.

# Usage
In project.clj:
```clojure
[luaclj "0.1.3"]
```

And in target namespace:
```clojure
(ns example.core
  (:require [luaclj.core :refer :all]
            [luaclj.library :refer :all]
            ))
```

luaclj.core namespace exports several functions:
```clojure
(lua->clj "local v = 0; for i = 1,100 do v = v + i end return v")
```
Result:
```clojure
(clojure.core/fn
 anonymous-chunk
 []
 (luaclj.util/process-return
  (luaclj.proteus/let-mutable
   [v
    0
    _
    (luaclj.util/process-break
     (clojure.core/doseq
      [i (clojure.core/range 1 (clojure.core/inc 100))]
      (do (set! v (+ v i)))))
    _
    (return v)])))
```
`process-return` and `process-break` are internal macros that deal with return and break statements.

`lua->clj` also accepts two optional keyword parameters: :fns and :nowrap:

`:fns` tells the parser that a list of function definitions is excepted, so for instance
```clojure
  (lua->clj
    "function f1(arg1, arg2)
      return arg1 + arg2
     end

     function f2(x,y,z)
       return x^y*z
     end"
    :fns)
```
will yield:
```clojure
((def
  f1
  (clojure.core/fn
   f1
   [arg1 arg2]
   (luaclj.util/process-return (return (+ arg1 arg2)))))
 (def
  f2
  (clojure.core/fn
   f2
   [x y z]
   (luaclj.util/process-return (return (* (expt x y) z))))))
```

`:nowrap` tells parser not to wrap generated code into an anonymous fn. Compare:
```clojure
(lua->clj "return 8^8")
```
```clojure
(clojure.core/fn anonymous-chunk [] (luaclj.util/process-return (return (expt 8 8))))
```
and
```clojure
(lua->clj "return 8^8" :nowrap)
```
(luaclj.util/process-return (return (expt 8 8)))

`eval-lua` will invoke `lua->clj` and then eval it in context of current namespace
```clojure
(eval-lua "return 2^3+3")
```
Result:

```clojure
#function[/eval141857/anonymous-chunk--141858] ;your results may differ
```

There is also a `lua` macro that can serve as an infix syntax helper. For instance:
```clojure
(lua if 3 < 5 then return "true" else return "false" end)
```
There are some caveats, however: all commas, table constructors and accessors should be surrounded with quotes. Otherwise Clojure reader will ignore commas and complain when tables contain odd number of entries. So the following sample Lua code:
```lua
sum = 0
for j = 1, 99 do
  sum = sum + j
end

t = {1, 10, 30}
t['a'] = 9
```
has to be presented as:
```clojure
sum = 0
for j = 1"," 99 do
  sum = sum + j
end

t = "{1, 10, 30}"
t"['a']" = 9
```

# Supported language features
  - Local and global variable declarations
  - if, while, repeat, for (with break statements)
  - function calls/definitions, both local and global
  - tables
  - correct operator precedence

# Not supported yet
  - metatables
  - varargs
  - standard library functions, except for `pairs` and `ipairs`
  - multiple return values from functions

# Acknowledgments
Special thanks goes to Mark Engelberg ([Instaparse](https://github.com/Engelberg/instaparse)), Zach Tellman ([Proteus](https://github.com/ztellman/proteus)), and Nathan Marz ([Specter](https://github.com/nathanmarz/specter/)). The library incorporates a slightly modified version of Proteus.

# License

Distributed under the MIT License.
