### What

A CLI parsing library offering out-of-the-box support for command hierarchies,
middleware, option validation / parsing, positional argument validation / parsing, and automatic help generation.

This library does *not* wrap `tools.cli` and instead implements its own command parsing from scratch.

This library does *not* use `clojure.spec` for option validation and instead uses Malli.

### Why

I love Malli; I value middleware and command hierarchies; and users want good error messages.


### Example Usage

```clojure 
(require '[io.github.rutledgepaulv.cli.core :as cli])

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
   :subcommands #{AddCommand}})

(def MainCommand
  {:command     "main"
   :description "This is the main command."
   :middleware  (fn middleware [handler outer-options]
                  (fn new-handler [inner-options]
                    (let [start-time (System/currentTimeMillis)
                          response   (handler (merge outer-options inner-options))
                          stop-time  (System/currentTimeMillis)]
                      (println "command took: " (- stop-time start-time) "milliseconds")
                      response)))
   :subcommands #{MathCommand}})

; if you already have parsed arguments from a shell (like inside -main)
(cli/run MainCommand ["main" "math" "add" "-a" "1" "-b" "2"])

; if you want to pass a single string and simulate the parsing done by a shell
(cli/run-string MainCommand "main math add -a 1 -b 2")

; help subcommands are implemented for you on 'branch' commands
(cli/run-string MainCommand "main help")

; help flags are implemented for you on 'leaf' commands
(cli/run-string MainCommand "main math add -h")

; you can show the entire command tree at once
(cli/run-string MainCommand "main tree")

```