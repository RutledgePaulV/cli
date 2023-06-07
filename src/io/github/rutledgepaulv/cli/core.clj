(ns io.github.rutledgepaulv.cli.core
  (:require [io.github.rutledgepaulv.cli.impl.explanations :as explanations]
            [io.github.rutledgepaulv.cli.impl.injections :as injections]
            [io.github.rutledgepaulv.cli.impl.schemas :as schemas]
            [io.github.rutledgepaulv.cli.impl.utils :as utils]
            [io.github.rutledgepaulv.cli.impl.ir :as ir]))


(defn summarize
  ([ir]
   (summarize ir (ir/find-root-node-id ir)))
  ([ir node-id]
   (explanations/summarize (injections/inject-help ir) node-id)))

(defn parse [command-tree args]
  (let [ast (-> command-tree
                (ir/command-tree->ir)
                (injections/inject-help))]
    (loop [node      ast
           parent    nil
           remaining args
           errors    []
           lens      (fn [x] x)]
      (if (empty? remaining)
        {:config    node
         :options   {}
         :arguments []
         :parent    {}
         :errors    errors}
        (let [candidate-options          (utils/indexcat-by :aliases (:options node))
              candidate-required-options (->> (:options node)
                                              (filter (comp schemas/is-required? :schema))
                                              (utils/indexcat-by :aliases))
              candidate-subcommands      (utils/index-by :command (:subcommands node))]

          )))))


(defn execute [parse-result]
  )

