(ns io.github.rutledgepaulv.cli.core-test
  (:require [clojure.string :as strings]
            [clojure.test :refer :all]
            [io.github.rutledgepaulv.cli.core :as cli]))

(def AddCommand
  {:command     "add"
   :run         (fn [{:keys [a b]}] (+ a b))
   :description "Adds two numbers together."
   :options     {:a {:description "The first number to add."
                     :aliases     #{"-a" "--alpha"}
                     :schema      [:and :int [:fn {:error/message "must be greater than 0"} pos?]]}
                 :b {:description "The second number to add."
                     :aliases     #{"-b" "--beta"}
                     :schema      [:and :int [:fn {:error/message "must be greater than 0"} pos?]]}}})

(def PositionalAddCommand
  {:command     "addp"
   :run         (fn [{:keys [arguments]}] (reduce + arguments))
   :description "Adds many numbers together."
   :arguments   [:+ [:and :int [:fn {:error/message "must be greater than 0"} pos?]]]})

(def MathCommand
  {:command     "math"
   :description "This is the math command."
   :options     {:a {:description "The offset."
                     :aliases     #{"-d" "--delta"}
                     :schema      :int}}
   :middleware  (fn [handler options] handler)
   :subcommands #{AddCommand PositionalAddCommand}})

(def MainCommand
  {:command     "main"
   :description "This is the main command."
   :subcommands #{MathCommand}})

(deftest test-top-level-summaries
  (is (= ["usage:"
          ""
          "  main <subcommand>"
          ""
          ""
          "subcommands:"
          ""
          "  help\t - Show help documentation for these commands."
          "  math\t - This is the math command."
          "  tree\t - Show the full command tree."]
         (strings/split-lines (cli/summarize MainCommand))))

  (is (= ["usage:"
          ""
          "  math [math-options] <subcommand>"
          ""
          ""
          "subcommands:"
          ""
          "  add\t - Adds two numbers together."
          "  addp\t - Adds many numbers together."
          "  help\t - Show help documentation for these commands."
          "  tree\t - Show the full command tree."
          ""
          ""
          "[math-options]"
          ""
          "  -d,--delta\tThe offset."]
         (strings/split-lines (cli/summarize MathCommand))))

  (is (= ["usage:"
          ""
          "  add [add-options] <subcommand>"
          ""
          ""
          "subcommands:"
          ""
          "  help\t - Show help documentation for these commands."
          "  tree\t - Show the full command tree."
          ""
          ""
          "[add-options]"
          ""
          "  -a,--alpha\tThe first number to add."
          "  -b,--beta\tThe second number to add."]
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
         (strings/split-lines (cli/summarize MainCommand "main.math.add")))))


(deftest parse-test
  (is (= [] (:path (cli/parse MainCommand []))))

  (is (= [{:command "main", :options {}, :errors []}
          {:command "main.help", :options {}, :errors [], :arguments []}]
         (:path (cli/parse MainCommand ["main" "help"]))))

  (is (= [{:command "main", :options {}, :errors []}
          {:command "main.math", :options {:a "test"}, :errors []}
          {:command "main.math.add", :options {:a "2", :b "6"}, :errors [], :arguments ["7"]}]
         (:path (cli/parse MainCommand ["main" "math" "-d" "test" "add" "-a" "2" "-b" "4" "-b" "6" "7"])))))
