(ns io.github.rutledgepaulv.cli.impl.schemas
  (:require [clojure.string :as strings]
            [io.github.rutledgepaulv.cli.impl.parsers :as parsers]
            [malli.core :as m]
            [malli.error :as me]
            [malli.transform :as mt]))

(def MalliSchema
  [:fn {:error/message "must be a valid malli schema"}
   (fn [x]
     (try
       (m/schema? (m/schema x))
       (catch Exception e false)))])

(defn get-parser-schema []
  (into [:enum] (remove #{:default} (keys (methods parsers/parse)))))

(def NotBlankString
  [:and :string [:fn {:error/message "must not be blank"} (complement strings/blank?)]])

(def StringWithoutWhitespace
  [:and NotBlankString
   [:re {:error/message "must not include any whitespace characters"} #"^[^\s]*$"]])

(def ShortOptionFlag
  [:and StringWithoutWhitespace
   [:fn {:error/message "single character options must be prefixed by a single dash"}
    (fn [x] (and (strings/starts-with? x "-") (= 2 (count x))))]])

(def LongOptionFlag
  [:and StringWithoutWhitespace
   [:fn {:error/message "multi character options must be prefixed by two dashes"}
    (fn [x] (and (strings/starts-with? x "--") (<= 4 (count x))))]])

(def OptionFlag
  [:or LongOptionFlag ShortOptionFlag])

(def OptionParser
  [:and :keyword
   [:fn {:error/fn
         (fn [{:keys [value]} _]
           (strings/join " " (me/humanize (m/explain (get-parser-schema) value))))}
    (fn [x] (m/validate (get-parser-schema) x))]])

(def OptionAliases
  [:and [:set {:default #{}} OptionFlag]
   [:fn {:error/message "must not be empty"} not-empty]])

(def Option
  [:map {:closed true}
   [:description NotBlankString]
   [:aliases OptionAliases]
   [:parser OptionParser]
   [:schema MalliSchema]])

(def CommandOptions
  [:and
   [:map-of {:default {}} :keyword Option]
   [:fn {:error/message "all options for a command must have distinct aliases"}
    (fn [x] (apply distinct? (mapcat :aliases (vals x))))]])

(def CommandMiddleware
  [:fn {:error/message "must be a middleware function that accepts a handler function and a map of options."} ifn?])

(def CommandFunction
  [:fn {:error/message "must be a function that accepts a map of options."} ifn?])

(def Command
  [:schema
   {:registry
    {::command
     [:and
      [:map {:closed true}
       [:command StringWithoutWhitespace]
       [:description NotBlankString]
       [:run {:optional true} CommandFunction]
       [:arguments {:optional true} MalliSchema]
       [:options {:optional true} CommandOptions]
       [:middleware {:optional true} CommandMiddleware]
       [:subcommands {:optional true}
        [:and [:set {:default #{}} [:ref ::command]]
         [:fn {:error/message "must have at least one subcommand"} not-empty]]]]
      [:fn {:error/message "must have either a run function or subcommands"}
       (fn [x] (or (and (contains? x :run) (not (contains? x :subcommands)))
                   (and (not (contains? x :run)) (contains? x :subcommands))))]
      [:fn {:error/message "can only have positional arguments on leaf commands with a run function"}
       (fn [x] (or (some? (:run x)) (not (some? (:arguments x)))))]]}}
   [:ref ::command]])


(def command-validator
  (m/validator Command))

(def option-validator
  (m/validator OptionFlag))

(defn is-command? [x]
  (command-validator x))

(defn is-option? [x]
  (option-validator x))

(defn is-multi-option? [schema]
  (or (contains? #{:set :vector} (m/type schema))
      (and (or (m/validate schema #{})
               (m/validate schema []))
           (and (not (m/validate schema 1))
                (not (m/validate schema "x"))))))

(defn is-branch? [x]
  (and (command-validator x) (not (empty? (:subcommands x)))))

(defn has-default? [schema]
  (some? (m/decode schema nil mt/default-value-transformer)))

(defn is-required? [schema]
  (and (not (has-default? schema)) (not (m/validate schema nil))))

(defn is-optional? [schema]
  (not (is-required? schema)))

(defn is-boolean? [schema]
  (or (contains? #{:boolean} (m/type schema))
      (and (m/validate schema true)
           (m/validate schema false)
           (not (m/validate schema "x"))
           (not (m/validate schema :x))
           (not (m/validate schema 1))
           (not (m/validate schema {}))
           (not (m/validate schema [])))))

(defn coerce [schema x]
  (m/decode schema x
            (mt/transformer
              mt/default-value-transformer
              mt/string-transformer
              mt/collection-transformer)))