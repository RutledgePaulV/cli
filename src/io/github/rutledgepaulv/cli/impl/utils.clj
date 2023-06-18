(ns io.github.rutledgepaulv.cli.impl.utils
  (:require [clojure.string :as strings]
            [malli.error :as me])
  (:import (java.io StringReader StringWriter)))


(defn indexcat-by [f coll]
  (into {} (mapcat (fn [x] (for [key (f x)] [key x]))) coll))

(defn index-by [f coll]
  (into {} (map (fn [x] [(f x) x])) coll))

(defn humanize [explanation]
  (me/humanize explanation {:wrap #(select-keys % [:value :message])}))


(defn tokenize
  "Tokenize string to list of individual space separated arguments.
  If argument contains space you can wrap it with `'` or `\"`."
  [s]
  (loop [s                 (StringReader. s)
         in-double-quotes? false
         in-single-quotes? false
         buf               (StringWriter.)
         parsed            []]
    (let [c (.read s)]
      (cond
        (= -1 c) (if (pos? (count (str buf)))
                   (conj parsed (str buf))
                   parsed)
        (= 39 c)                                            ;; single-quotes
        (if in-double-quotes?
          (recur s in-double-quotes? false (doto buf
                                             (.write c)) parsed)
          (if in-single-quotes?
            (recur s in-double-quotes? false (StringWriter.) (conj parsed (str buf)))
            (recur s in-double-quotes? true buf parsed)))
        (= 92 c)                                            ;; the \\ escape character
        (let [escaped (.read s)
              buf     (if (and in-double-quotes?
                               (= 34 escaped))              ;; double quote
                        (doto buf (.write escaped))
                        (doto buf
                          (.write c)
                          (.write escaped)))]
          (recur s in-double-quotes? in-single-quotes? buf parsed))

        (and (not in-single-quotes?) (= 34 c))              ;; double quote
        (if in-double-quotes?
          ;; exit double-quoted string
          (recur s false in-single-quotes? buf parsed)
          ;; enter double-quoted string
          (recur s true in-single-quotes? buf parsed))

        (and (not in-double-quotes?)
             (not in-single-quotes?)
             (Character/isWhitespace c))
        (recur s in-double-quotes? in-single-quotes? (StringWriter.)
               (let [bs (str buf)]
                 (cond-> parsed
                   (not (strings/blank? bs)) (conj bs))))
        :else (do
                (.write buf c)
                (recur s in-double-quotes? in-single-quotes? buf parsed))))))
