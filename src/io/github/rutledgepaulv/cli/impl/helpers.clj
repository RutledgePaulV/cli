(ns io.github.rutledgepaulv.cli.impl.helpers
  (:require [clojure.string :as strings]
            [io.github.rutledgepaulv.cli.impl.ir :as ir]
            [io.github.rutledgepaulv.cli.impl.schemas :as schemas]))

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

(defn inject-help-subcommands [{:keys [nodes] :as ir}]
  (reduce
    (fn [{:keys [forward-graph] :as ir} node-id]
      (if (not-empty (get forward-graph node-id))
        (let [help-command-id (random-uuid)
              help-command    (with-meta
                                {:command     "help"
                                 :description "Show help documentation for these commands."
                                 :options     {}
                                 :run         (fn [_] [:documentation (summarize ir node-id)])}
                                {:id help-command-id :parent node-id})]
          (-> ir
              (assoc-in [:nodes help-command-id] help-command)
              (update-in [:forward-graph node-id] (fnil conj #{}) help-command-id)
              (assoc-in [:reverse-graph help-command-id] node-id)))
        ir))
    ir
    (remove #{:root} (keys nodes))))

(defn inject-help-options [{:keys [nodes] :as ir}]
  (reduce
    (fn [{:keys [forward-graph] :as ir} node-id]
      (if (empty? (get forward-graph node-id))
        (let [help-option {:description "Show help documentation for this command."
                           :parser      :boolean
                           :aliases     #{"-h" "--help"}
                           :schema      [:maybe {:default false} :boolean]}]
          (-> ir
              (assoc-in [:nodes node-id :options :help] help-option)
              (update-in [:nodes node-id :run]
                         (fn new-run [old-run]
                           (fn [options]
                             (if (:help options)
                               [:documentation (summarize ir node-id)]
                               (old-run options)))))))
        ir))
    ir
    (remove #{:root} (keys nodes))))

(defn inject-help [ir]
  (-> ir
      (inject-help-options)
      (inject-help-subcommands)))

(defn required-option? [option]
  (schemas/is-required? (:schema option)))