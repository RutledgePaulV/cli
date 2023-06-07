(ns io.github.rutledgepaulv.cli.impl.ir
  "Command trees are clumsy to deal with because we have to traverse both forwards
   and backwards through the tree to display help / execute middleware, etc. So,
   instead we introduce an intermediate representation which is an index of commands
   and adjacency graphs describing the parent and child relationships between them."
  (:require [clojure.walk :as walk]
            [io.github.rutledgepaulv.cli.impl.schemas :as schemas]))

(defn node->id [node]
  (if (uuid? node) node (:id (meta node))))

(defn parent-of [{:keys [nodes reverse-graph] :as ir} node]
  (get nodes (get reverse-graph (node->id node))))

(defn children-of [{:keys [nodes forward-graph] :as ir} node]
  (set (map nodes (get forward-graph (node->id node) #{}))))

(defn ancestor-seq [{:keys [nodes] :as ir} root]
  (let [branch (memoize (fn [node] (disj (hash-set (parent-of ir node)) nil)))]
    (tree-seq (comp not-empty branch) branch (get nodes root))))

(defn descendant-seq [{:keys [nodes] :as ir} root]
  (let [branch (memoize (partial children-of ir))]
    (tree-seq (comp not-empty branch) branch (get nodes root))))

(defn find-root-node-id [ir]
  (first (get-in ir [:forward-graph :root])))

(defn find-node-id-by-command-pred [ir pred]
  (reduce (fn [nf [k v]]
            (if (pred v)
              (reduced k)
              nf))
          nil
          (:nodes ir)))

(defn has-parent? [ir node]
  (some? (parent-of ir node)))

(defn has-child? [ir node]
  (not-empty (children-of ir node)))

(defn assign-command-ids [command]
  (letfn [(assign [form]
            (if (schemas/is-command? form)
              (vary-meta form update :id #(or % (random-uuid)))
              form))]
    (walk/postwalk assign command)))

(defn command-tree->ir [config]
  (->> (assign-command-ids config)
       (tree-seq schemas/is-branch?
                 (fn [branch]
                   (let [parent (node->id branch)]
                     (->> (:subcommands branch)
                          (map (fn [x] (vary-meta x assoc :parent parent)))))))
       (map (fn [x] (dissoc x :subcommands)))
       (reduce (fn [agg x]
                 (-> agg
                     (assoc-in [:nodes (node->id x)] x)
                     (update-in [:forward-graph (:parent (meta x) :root)] (fnil conj #{}) (node->id x))
                     (assoc-in [:reverse-graph (node->id x)] (:parent (meta x) :root))))
               {:nodes         {}
                :forward-graph {}
                :reverse-graph {}})))