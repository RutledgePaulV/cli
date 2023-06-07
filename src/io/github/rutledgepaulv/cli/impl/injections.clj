(ns io.github.rutledgepaulv.cli.impl.injections
  "Functions that modify the intermediate representation to inject
   additional functionality beyond what was specified by the user
   in their initial command-tree."
  (:require [io.github.rutledgepaulv.cli.impl.explanations :as explanations]))

(defn inject-help-subcommands [{:keys [nodes] :as ir}]
  (reduce
    (fn [{:keys [nodes forward-graph] :as ir} node-id]
      (if (not-empty (get forward-graph node-id))
        (let [help-command-id (str (:id (meta (get nodes node-id))) ".help")
              help-command    (with-meta
                                {:command     "help"
                                 :description "Show help documentation for these commands."
                                 :options     {}
                                 :run         (fn [_] [:documentation (explanations/summarize ir node-id)])}
                                {:id help-command-id :parent node-id})]
          (-> ir
              (assoc-in [:nodes help-command-id] help-command)
              (update-in [:forward-graph node-id] (fnil conj #{}) help-command-id)
              (assoc-in [:reverse-graph help-command-id] node-id)))
        ir))
    ir
    (remove #{""} (keys nodes))))

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
                               [:documentation (explanations/summarize ir node-id)]
                               (old-run options)))))))
        ir))
    ir
    (remove #{""} (keys nodes))))

(defn inject-help [ir]
  (-> ir
      (inject-help-options)
      (inject-help-subcommands)))