(ns io.github.rutledgepaulv.cli.core
  (:require [io.github.rutledgepaulv.cli.impl.explanations :as explanations]
            [io.github.rutledgepaulv.cli.impl.injections :as injections]
            [io.github.rutledgepaulv.cli.impl.ir :as ir]
            [io.github.rutledgepaulv.cli.impl.schemas :as schemas]
            [io.github.rutledgepaulv.cli.impl.utils :as utils]
            [clojure.pprint :as pprint]
            [malli.core :as m]
            [malli.error :as me]))

(defn validate! [command-tree]
  (if-some [explanation (m/explain schemas/Command command-tree)]
    (let [human-readable (me/humanize explanation)]
      (throw (ex-info (str "Invalid command tree." \newline \newline
                           (with-out-str (pprint/pprint human-readable)))
                      {:explanation human-readable})))))

(defn command-tree->ir [command-tree]
  (validate! command-tree)
  (-> command-tree
      (ir/command-tree->ir)
      (injections/inject-tree)
      (injections/inject-help)))

(defn summarize
  ([command-tree]
   (let [ir (command-tree->ir command-tree)]
     (explanations/summarize (injections/inject-help ir) (ir/find-root-node-id ir))))
  ([command-tree node-id]
   (let [ir (command-tree->ir command-tree)]
     (explanations/summarize (injections/inject-help ir) node-id))))

(defn parse [command-tree args]
  (let [{:keys [forward-graph nodes] :as ir} (command-tree->ir command-tree)]
    (loop [remaining args path [] candidate-commands (utils/index-by :command (vals (select-keys nodes [(ir/find-root-node-id ir)])))]
      (if (or (empty? remaining) (empty? candidate-commands))
        {:ir ir :path path}
        (let [next-candidate (first remaining)]
          (if-some [command (get candidate-commands next-candidate)]
            (let [candidate-options (utils/indexcat-by :aliases (for [[k v] (:options command)] (assoc v :key k)))
                  command-opts      (loop [[option-name option-value & more :as remainder] (rest remaining) options {} errors []]
                                      (if (empty? remainder)
                                        {:options options :errors errors}
                                        (if (schemas/is-option? option-name)
                                          (if-some [option-spec (get candidate-options option-name)]
                                            (cond
                                              (nil? option-value)
                                              (recur more (assoc options (:key option-spec) true) errors)
                                              (schemas/is-option? option-value)
                                              (recur (cons option-value more) (assoc options (:key option-spec) true) errors)
                                              :otherwise
                                              (if (schemas/is-multi-option? (:schema option-spec))
                                                (recur more (update options (:key option-spec) (fnil conj #{}) option-value) errors)
                                                (recur more (assoc options (:key option-spec) option-value) errors)))
                                            (recur (cons option-value more) options (conj errors {:kind :unknown-option :option option-name})))
                                          {:options options :errors errors :remainder remainder})))]
              (if (empty? (get forward-graph (:id (meta (get candidate-commands next-candidate)))))
                {:ir ir :path (conj path {:command   (:id (meta (get candidate-commands next-candidate)))
                                          :options   (:options command-opts)
                                          :errors    (:errors command-opts)
                                          :arguments (vec (:remainder command-opts []))})}
                (let [candidates (get forward-graph (:id (meta (get candidate-commands next-candidate))))]
                  (recur (:remainder command-opts)
                         (conj path {:command (:id (meta (get candidate-commands next-candidate)))
                                     :options (:options command-opts)
                                     :errors  (:errors command-opts)})
                         (utils/index-by :command (vals (select-keys nodes candidates)))))))
            {:ir ir :path (conj path {:errors [{:kind :unknown-command :command next-candidate}]})}))))))


(defn coerce [{:keys [ir path] :as parse-result}]
  (letfn [(coerce [{:keys [command options arguments] :as segment}]
            (let [spec            (get-in ir [:nodes command])
                  coerced-options (reduce
                                    (fn [options [option-key option-value]]
                                      (assoc
                                        options
                                        option-key
                                        (schemas/coerce
                                          (get-in spec [:options option-key :schema])
                                          option-value)))
                                    {}
                                    options)]
              (cond-> segment
                :always
                (assoc :options coerced-options)
                (some? (:arguments spec))
                (assoc :arguments (schemas/coerce (get-in spec [:arguments :schema]) arguments)))))]
    {:ir ir :path (reduce (fn [path segment] (conj path (coerce segment))) [] path)}))

(defn validate [{:keys [ir path] :as parse-result}]
  parse-result)

(defn default-middleware [handler options]
  (fn inner-handler [inner-options]
    (handler (merge options inner-options))))

(defn check [{:keys [ir path] :as parse-result}]
  parse-result)

(defn execute [{:keys [ir path] :as parse-result}]
  (let [[[spec parsed] & upstream]
        (->> path
             rseq
             (map (fn [{:keys [command] :as parsed}]
                    [(get-in ir [:nodes command]) parsed])))]
    (try
      [:result
       ((reduce
          (fn [handler [spec parsed]]
            ((:middleware spec default-middleware) handler (:options parsed)))
          ((:middleware spec default-middleware)
           (:run spec)
           (assoc (:options parsed) :arguments (:arguments parsed)))
          upstream)
        {})]
      (catch Exception e
        [:error e]))))


(defn run [command-tree args]
  (-> (parse command-tree args)
      (coerce)
      (validate)
      (check)
      (execute)))

(defn run-string [command-tree arg-string]
  (run command-tree (clojure.string/split arg-string #"\s+")))

(comment

  (execute (parse MainCommand ["main" "math" "add" "-a" "1" "-b" "2"]))

  )