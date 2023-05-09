(ns io.github.rutledgepaulv.cli.impl.parsers
  (:require [clojure.edn :as edn]
            [clojure.string :as strings]))

(defn dispatch [{:keys [parser]} argument]
  parser)

(defmulti parse #'dispatch)

(defmethod parse :default [option argument]
  argument)

(defmethod parse :number [option argument]
  (try
    (Long/parseLong argument)
    (catch NumberFormatException e
      (Double/parseDouble argument))))

(defmethod parse :boolean [option argument]
  (get {"yes"   true
        "y"     true
        "1"     true
        "true"  true
        "no"    false
        "n"     false
        "0"     false
        "false" false}
       (strings/lower-case argument)
       argument))

(defmethod parse :edn [option argument]
  (edn/read-string {:readers *data-readers*} argument))

(defn parse* [option argument]
  (try
    (parse option argument)
    (catch Exception e argument)))