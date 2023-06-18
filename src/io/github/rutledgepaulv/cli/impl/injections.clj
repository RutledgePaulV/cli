(ns io.github.rutledgepaulv.cli.impl.injections
  "Functions that modify the intermediate representation to inject
   additional functionality beyond what was specified by the user
   in their initial command-tree."
  (:require [io.github.rutledgepaulv.cli.impl.explanations :as explanations]
            [io.github.rutledgepaulv.cli.impl.ir :as ir]))

(defn inject-help-subcommands [{:keys [nodes] :as ir}]
  (reduce
    (fn [{:keys [nodes forward-graph] :as ir} node-id]
      (if (not-empty (get forward-graph node-id))
        (let [help-command-id (str (:id (meta (get nodes node-id))) ".help")
              injected-ir     (promise)
              help-command    (with-meta
                                {:command     "help"
                                 :description "Show help documentation for these commands."
                                 :options     {}
                                 :run         (fn [_] [:documentation (explanations/summarize (deref injected-ir) node-id)])}
                                {:id help-command-id :parent node-id})
              new-ir          (-> ir
                                  (assoc-in [:nodes help-command-id] help-command)
                                  (update-in [:forward-graph node-id] (fnil conj #{}) help-command-id)
                                  (assoc-in [:reverse-graph help-command-id] node-id))]
          (deliver injected-ir new-ir)
          new-ir)
        ir))
    ir
    (keys nodes)))

(defn inject-help-options [{:keys [nodes] :as ir}]
  (reduce
    (fn [{:keys [forward-graph] :as ir} node-id]
      (if (empty? (get forward-graph node-id))
        (let [help-option {:description "Show help documentation for this command."
                           :parser      :boolean
                           :aliases     #{"-h" "--help"}
                           :schema      [:maybe {:default false} :boolean]}
              injected-ir (promise)
              new-ir      (-> ir
                              (assoc-in [:nodes node-id :options :help] help-option)
                              (update-in [:nodes node-id :run]
                                         (fn new-run [old-run]
                                           (fn [options]
                                             (if (:help options)
                                               [:documentation (explanations/summarize (deref injected-ir) node-id)]
                                               (old-run options))))))]
          (deliver injected-ir new-ir)
          new-ir)
        ir))
    ir
    (keys nodes)))

(defn inject-help [ir]
  (-> ir
      (inject-help-options)
      (inject-help-subcommands)))

(defn inject-tree [ir]
  (let [root-id      (ir/find-root-node-id ir)
        hiccup       (ir/ir->hiccup ir)
        tree-id      (str root-id ".tree")
        tree-command (with-meta
                       {:command     "tree"
                        :description "Show the full command tree."
                        :options     {:depth {:description "How many levels deep should be displayed?"
                                              :parser      :number
                                              :aliases     #{"-d" "--depth"}
                                              :schema      [:maybe {:default Integer/MAX_VALUE} 'pos-int?]}}
                        :run         (fn [_] [:documentation (explanations/hiccup->tree hiccup)])}
                       {:id tree-id :parent root-id})]
    (-> ir
        (assoc-in [:nodes tree-id] tree-command)
        (update-in [:forward-graph root-id] (fnil conj #{}) tree-id)
        (assoc-in [:reverse-graph tree-id] root-id))))