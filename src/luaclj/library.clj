(ns luaclj.library)



(defn pairs [d]
  (seq d))

(def ipairs pairs)
