(ns luaclj.library
  (:require [clojure.math.numeric-tower :as math]
            ))


(def expt math/expt)
(def floor math/floor)

(defn pairs [d]
  (seq d))

(def ipairs pairs)
