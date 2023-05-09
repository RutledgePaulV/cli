(ns io.github.rutledgepaulv.cli.core-test
  (:require [clojure.string :as strings]
            [clojure.test :refer :all]
            [io.github.rutledgepaulv.cli.core :as cli]
            [io.github.rutledgepaulv.cli.impl.helpers :as helpers]))

(def AddCommand
  {:command     "add"
   :run         (fn [{:keys [a b]}] (+ a b))
   :description "Adds two numbers together."
   :options     {:a {:description "The first number to add."
                     :parser      :number
                     :aliases     #{"-a" "--alpha"}
                     :schema      [:and :int [:fn {:error/message "must be greater than 0"} pos?]]}
                 :b {:description "The second number to add."
                     :parser      :number
                     :aliases     #{"-b" "--beta"}
                     :schema      [:and :int [:fn {:error/message "must be greater than 0"} pos?]]}}})

(def MathCommand
  {:command     "math"
   :description "This is the math command."
   :options     {:a {:description "The offset."
                     :parser      :number
                     :aliases     #{"-d" "--delta"}
                     :schema      :int}}
   :middleware  (fn [handler options] handler)
   :subcommands #{AddCommand}})

(def MainCommand
  {:command     "main"
   :description "This is the main command."
   :subcommands #{MathCommand}})

(defn extract-in [x path]
  (loop [x x path path]
    (if (empty? path)
      x
      (if (fn? (first path))
        (recur (first (filter (first path) x)) (rest path))
        (recur (get x (first path)) (rest path))))))

(deftest test-top-level-summaries
  (is (= ["usage:"
          ""
          "  main <subcommand>"
          ""
          ""
          "subcommands:"
          ""
          "  help\t - Show help documentation for these commands."
          "  math\t - This is the math command."]
         (strings/split-lines (cli/summarize MainCommand))))

  (is (= ["usage:"
          ""
          "  math [math-options] <subcommand>"
          ""
          ""
          "subcommands:"
          ""
          "  add\t - Adds two numbers together."
          "  help\t - Show help documentation for these commands."
          ""
          ""
          "[math-options]"
          ""
          "  -d,--delta\tThe offset."]
         (strings/split-lines (cli/summarize MathCommand))))

  (is (= ["usage:"
          ""
          "  add [add-options]"
          ""
          ""
          "[add-options]"
          ""
          "  -a,--alpha\tThe first number to add."
          "  -b,--beta\tThe second number to add."
          "  -h,--help\tShow help documentation for this command."]
         (strings/split-lines (cli/summarize AddCommand)))))


(deftest test-nested-summaries
  (is (= ["usage:"
          ""
          "  main math [math-options] add [add-options]"
          ""
          ""
          "[add-options]"
          ""
          "  -a,--alpha\tThe first number to add."
          "  -b,--beta\tThe second number to add."
          "  -h,--help\tShow help documentation for this command."
          ""
          ""
          "[math-options]"
          ""
          "  -d,--delta\tThe offset."]
         (->> [:subcommands #(= "math" (:command %))
               :subcommands #(= "add" (:command %))]
              (extract-in (helpers/config->ast MainCommand))
              (cli/summarize)
              (strings/split-lines)))))