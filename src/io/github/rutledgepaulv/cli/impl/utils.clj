(ns io.github.rutledgepaulv.cli.impl.utils)


(defn indexcat-by [f coll]
  (into {} (mapcat (fn [x] (for [key (f x)] [key x]))) coll))

(defn index-by [f coll]
  (into {} (map (fn [x] [(f x) x])) coll))