# luaclj

This is a Lua-to-Clojure parser. It takes Lua source code string as an input and produces eval'able Clojure code.

# Usage
In project.clj:
```clojure
[luaclj "0.1.0"]
```

And in target namespace:
```clojure
(ns example.core
  (:require [luaclj.core :as lua]))
```

luaclj.core namespace exports several functions:
```clojure
(lua/parse-lua "local v = 0; for i = 1,100 do v = v + i end return v")
```
Result:


```clojure
(lua/create-lua-fn "return 2^3+3")
```
Result:

# Supported language features
  - Local and global variable declarations
  - if, while, repeat, for (with break statements)
  - function calls/definitions, both local and global
  - tables
  - correct operator precedence

# Not supported yet
  - metatables
  - standard library functions, except for `pairs` and `ipairs`

# Acknowledgments
Special thanks goes to Mark Engelberg ([Instaparse](https://github.com/Engelberg/instaparse)), Zach Tellman ([Proteus](https://github.com/ztellman/proteus)), and Nathan Marz ([Specter](https://github.com/nathanmarz/specter/)).

# License

Distributed under the MIT License.
