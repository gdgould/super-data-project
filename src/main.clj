(ns main
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))


;; Assumptions:

;; Airline names comprise alphabetic characters only.  Numbers and punctuation will be removed
;; The DelayTimes column always contains a stringified array of integers.
;; Flight codes which exist will be in the correct sequence.
;;   Note: if they are not, this code will respect the existing flight code, and use it as
;;     the new baseline to increment from.


;;
;; String parser functions
;;

(defn trim-and-keep-alphanumeric
  "Given a string, return the same string, keeping only alphabetic 
   characters and internal spaces."
  [s]
  ;; Return nil for non-strings
  (when (string? s)
    (->> (re-seq #"[a-zA-Z ]" s)
         (apply str)
         (str/trim))))

(defn to-title-case
  "Converts a string to title case"
  [s]
  (when (string? s)
    (->> (str/split s #" ")
         (map str/capitalize)
         (str/join " "))))

;;
;; Desired schemas
;;

(def initial-schema
  [{:column-name :airline-code
    :parser trim-and-keep-alphanumeric}
   {:column-name :delay-times
    ;; read-string will read vectors like [1, 2] as valid clojure vectors of integers
    :parser edn/read-string}
   {:column-name :flight-code
    ;; Parse the float-as-string using bigdec, then cast to an integer.  Ignore nils
    :parser (fn [s] (some-> s bigdec int))}
   {:column-name :to-from
    ;; Don't parse for now.  We'll split this data further later
    :parser identity}])

(def parsed-to-from-schema
  [{:column-name :to
    :parser to-title-case}
   {:column-name :from
    :parser to-title-case}])

;;
;; Parsing functions and transducers
;;

(defn raw-data-line->map
  "Given a single line of data, a regex expression for the column separator,
     and a schema containing column names and parsers,
     returns a map from column names to parsed data.
   Interprets the empty string as nil."
  [column-schema re-separator raw-data]
  (->> (str/split raw-data re-separator)
       ;; Clojure-idiomatic 'zip'
       (map vector column-schema)
       ;; Parse each value, and assign it to the correct column name
       (map (fn [[{:keys [column-name parser]} value]]
              (let [parsed-value (when (and value (not= "" value))
                                   (parser value))]
                {column-name parsed-value})))
       (apply merge)))

(defn split-column
  "Splits the given string-valued column into one or more based on the given separator and schema.
   Returns a map excluding the split column, but including all columns from new-schema."
  [col-to-split new-schema re-separator m]
  (-> (dissoc m col-to-split)
      (merge (raw-data-line->map new-schema
                                 re-separator
                                 (get m col-to-split)))))

(defn fill-flight-codes-xf
  "Returns a stateful transducer which will fill in missing flight codes
     so they increase by 10 each row."
  []
  (fn [xf]
    (let [current-code (atom 0)]
      (fn
        ([] xf)
        ([result] (xf result))
        ([result input]
         (let [fcode (:flight-code input)
               ;; Use the input's code if it has one, else fall back to the previous code + 10
               _ (if fcode
                   (reset! current-code fcode)
                   (swap! current-code + 10))
               new-input (assoc input :flight-code @current-code)]
           (xf result new-input)))))))

;;
;; Main operations
;;

(defn parse-airline-data-table
  "Given the stringified data table, parse it."
  [table-as-str]
  (let [;; Drop the header line
        data-lines (->> (str/split table-as-str #"\n")
                        (drop 1))]

    ;; We will process the data using clojure's transducers,
    ;;   which create a clean, reusable data pipeline.
    ;; Transducers are like operations on a conveyor belt:
    ;;   they transform a piece of data, then put it back on the belt
    ;;   to be passed in to the next transducer.
    
    ;; The `fill-flight-codes-xf` transducer is "stateful".  That is,
    ;;   it remembers the flight code of the previous entry.  That way,
    ;;   it can add 10 to the previous flight code if it needs to fill in a null value.
    (into []
          (comp
           (map (partial raw-data-line->map initial-schema #";"))
           (map (partial split-column :to-from parsed-to-from-schema #"_"))
           (fill-flight-codes-xf))
          data-lines)))

;; Output will have the following format (see the tests for the full example)
;; [{:airline-code "Air Canada", :delay-times [21 40], :flight-code 20015, :to "Waterloo", :from "Newyork"}
;;  {...}
;;  ...]

;; Why this format?
;; In Clojure, this format may be immediately inserted into a SQL table,
;;   with map keys corresponding to SQL column names (possibly with a kebab->snake case translation layer).
;; Similarly, if a Clojure function expects data from a SQL table, it will require data to be in the above format.
;; Thus, in Clojure, this is a very idiomatic format for a data table.


(defn -main [stringified-table]
  (println (parse-airline-data-table stringified-table)))


;; I'm not sure if this is part of the project requirement, but 
;;   I've added it for completeness.

(defn parse-and-re-stringify
  "Parses the stringified table, but then re-stringifies."
  [table-as-str]
  (let [new-header "Airline Code;DelayTimes;FlightCodes;To;From"
        
        parsed (parse-airline-data-table table-as-str)
        parsed-as-str (->> parsed
                           (map #(map (comp str val) %))
                           (map (partial str/join ";"))
                           (str/join "\n"))]
    (str new-header "\n" parsed-as-str)))
