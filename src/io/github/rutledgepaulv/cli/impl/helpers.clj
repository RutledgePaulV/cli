(ns io.github.rutledgepaulv.cli.impl.helpers
  (:require [clojure.string :as strings]
            [clojure.walk :as walk]
            [io.github.rutledgepaulv.cli.impl.schemas :as schemas]))

(defn ast->command-seq [ast]
  (tree-seq (comp some? :parent) (comp vector :parent) ast))


(defn ast->usage [ast]
  (->> (ast->command-seq ast)
       (reverse)
       (reduce (fn [agg {:keys [command options subcommands]}]
                 (cond-> agg
                   (strings/blank? agg)
                   (str command)
                   (not (strings/blank? agg))
                   (str " " command)
                   (not-empty options)
                   (str (format " [%s-options]" command))
                   (not-empty subcommands)
                   (str " <subcommand>")
                   :always
                   (strings/replace #"<subcommand>\s+" "")))
               "")))

(defn command->options-table [{:keys [options command]}]
  (->>
    (into [(format "[%s-options]" command) ""]
          (for [{:keys [aliases schema description]} (sort-by (comp (partial apply max-key count) :aliases) (vals options))]
            (str "  "
                 (strings/join "," (sort-by (juxt count identity) aliases))
                 \tab
                 description)))
    (strings/join \newline)))

(defn summarize [ast]
  (strings/join \newline
                (into ["usage:" "" (str "  " (ast->usage ast))]
                      (->> (concat
                             (if (not-empty (:subcommands ast))
                               (into [\newline "subcommands:" ""]
                                     (for [{:keys [command description]} (sort-by :command (:subcommands ast))]
                                       (str "  " command \tab " - " description)))
                               [])
                             (->> (ast->command-seq ast)
                                  (remove (comp empty? :options))
                                  (map command->options-table)
                                  (interleave (repeat \newline))))
                           (remove nil?)))))

(defn config->ast [config]
  (letfn [(children [{:keys [subcommands] :as parent}]
            (set (map (fn [y] (assoc y :parent parent)) subcommands)))]
    (->> (tree-seq (comp schemas/is-branch? (fn [x] (dissoc x :parent))) children config)
         (reduce (fn [agg x] (walk/postwalk-replace {(dissoc x :parent) x} agg))))))

(defn create-help-command [ast-promise]
  {:command     "help"
   :description "Show help documentation for these commands."
   :options     {}
   :run         (fn [_] [:documentation (summarize (deref ast-promise))])})

(def HelpOption
  {:description "Show help documentation for this command."
   :parser      :boolean
   :aliases     #{"-h" "--help"}
   :schema      [:maybe {:default false} :boolean]})

(defn add-help-command [branch-command]
  (let [prom                  (promise)
        subcommand            (assoc (create-help-command prom) :parent branch-command)
        update-fn             (fn [subcommands]
                                (-> (set (remove (comp #{(:command subcommand)} :command) (or subcommands #{})))
                                    (conj subcommand)))
        form-with-subcommands (update branch-command :subcommands update-fn)]
    (deliver prom form-with-subcommands)
    form-with-subcommands))

(defn inject-help-subcommands [ast]
  (walk/postwalk
    (fn [form]
      (if (and (map? form) (not-empty (:subcommands form)))
        (add-help-command form)
        form))
    ast))

(defn inject-help-options [ast]
  (walk/postwalk
    (fn [form]
      (if (and (map? form) (schemas/is-leaf? (dissoc form :parent)))
        (let [form' (assoc-in form [:options :help] HelpOption)]
          (update form' :run (fn [old-handler]
                               (fn [options]
                                 (if (:help options)
                                   [:documentation (summarize form')]
                                   (old-handler options))))))
        form))
    ast))

(defn inject-help [ast]
  (-> ast
      (inject-help-subcommands)
      (inject-help-options)))

(defn required-option? [option]
  (schemas/is-required? (:schema option)))