(ns io.github.rutledgepaulv.cli.impl.explanations
  "Implements functions that reflect over the intermediate representation
   in order to generate help text for invoking the CLI being described."
  (:require [clojure.string :as strings]
            [io.github.rutledgepaulv.cli.impl.ir :as ir]))

(defn ast->usage
  ([ir] (ast->usage ir (ir/find-root-node-id ir)))
  ([ir node-id]
   (->> (ir/ancestor-seq ir node-id)
        (reverse)
        (reduce (fn [agg {:keys [command options] :as node}]
                  (cond-> agg
                    (strings/blank? agg)
                    (str command)
                    (not (strings/blank? agg))
                    (str " " command)
                    (not-empty options)
                    (str (format " [%s-options]" command))
                    (ir/has-child? ir node)
                    (str " <subcommand>")
                    :always
                    (strings/replace #"<subcommand>\s+" "")))
                ""))))

(defn command->options-table [{:keys [options command]}]
  (->>
    (into [(format "[%s-options]" command) ""]
          (for [{:keys [aliases description]} (sort-by (comp (partial apply max-key count) :aliases) (vals options))]
            (str "  " (strings/join "," (sort-by (juxt count identity) aliases)) \tab description)))
    (strings/join \newline)))

(defn summarize
  ([ir] (summarize ir (ir/find-root-node-id ir)))
  ([ir root]
   (->> (into ["usage:" "" (str "  " (ast->usage ir root))]
              (->> (concat
                     (let [children (ir/children-of ir root)]
                       (if (not-empty children)
                         (into [\newline "subcommands:" ""]
                               (for [{:keys [command description]} (sort-by :command children)]
                                 (str "  " command \tab " - " description)))
                         []))
                     (->> (ir/ancestor-seq ir root)
                          (remove (comp empty? :options))
                          (map command->options-table)
                          (interleave (repeat \newline))))
                   (remove nil?)))
        (strings/join \newline))))

(defn hiccup->tree [hiccup]
  (letfn [(emit [depth [node & children]]
            (println (str (apply str (repeat depth "  ")) node))
            (doseq [child (sort-by first children)]
              (emit (inc depth) child)))]
    (emit 0 hiccup)))
