(ns io.github.rutledgepaulv.cli.core
  (:require [io.github.rutledgepaulv.cli.impl.helpers :as helpers]
            [io.github.rutledgepaulv.cli.impl.utils :as utils]))


(defn summarize [config]
  (helpers/summarize
    (-> config
        (helpers/config->ast)
        (helpers/inject-help))))

(defn parse [config args]
  (let [ast (-> config
                (helpers/config->ast)
                (helpers/inject-help))]
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
                                              (filter helpers/required-option?)
                                              (utils/indexcat-by :aliases))
              candidate-subcommands      (utils/index-by :command (:subcommands node))]

          )))))


(defn execute [parse-result]
  )

