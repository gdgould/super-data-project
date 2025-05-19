(ns main-test 
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [main :as main]))

;; Note this data has been modified slightly.  Double quotes " must be escaped inside clojure strings.
(def sample-data "Airline Code;DelayTimes;FlightCodes;To_From\nAir Canada (!);[21, 40];20015.0;WAterLoo_NEWYork\n<Air France> (12);[];;Montreal_TORONTO\n(Porter Airways. );[60, 22, 87];20035.0;CALgary_Ottawa\n12. Air France;[78, 66];;Ottawa_VANcouvER\n\"\"\".\\.Lufthansa.\\.\"\"\";[12, 33];20055.0;london_MONTreal\n")

(def data-headers "Airline Code;DelayTimes;FlightCodes;To_From")

(defn generate-test-data-line
  [{:keys [airline delay-times flight-code to from]
    :or {airline "Air Canada"
         delay-times [1 2 3]
         flight-code 1000
         to "Waterloo"
         from "Vancouver"}}]
  
  (str/join ";"
            [airline
             (str "[" (str/join ", " delay-times) "]")
             flight-code
             (str to "_" from)]))

(deftest parse-airline-data-table-test
  (testing "Empty table"
    (is (= []
           (main/parse-airline-data-table data-headers))))

  (testing "Single line"
    (let [line1 (generate-test-data-line
                 {:airline-code "123 Air Canada **.\\"
                  :delay-times [2229 71]
                  :flight-code 171.00
                  :to "new YORK"
                  :from "LONDON HEathrow"})
          data (str/join "\n" [data-headers line1])]
      (is (= [{:airline-code "Air Canada", :delay-times [2229 71], :flight-code 171, :to "New York", :from "London Heathrow"}]
             (main/parse-airline-data-table data)))))

  (testing "Example data parses properly"
    (is (= [{:airline-code "Air Canada", :delay-times [21 40], :flight-code 20015, :to "Waterloo", :from "Newyork"}
            {:airline-code "Air France", :delay-times [], :flight-code 20025, :to "Montreal", :from "Toronto"}
            {:airline-code "Porter Airways", :delay-times [60 22 87], :flight-code 20035, :to "Calgary", :from "Ottawa"}
            {:airline-code "Air France", :delay-times [78 66], :flight-code 20045, :to "Ottawa", :from "Vancouver"}
            {:airline-code "Lufthansa", :delay-times [12 33], :flight-code 20055, :to "London", :from "Montreal"}]
           (main/parse-airline-data-table sample-data))))

  (testing "Multiple missing flight codes in a row"
    (let [line1 (generate-test-data-line {:flight-code 1220.0})
          line2 (generate-test-data-line {:flight-code nil})
          line3 (generate-test-data-line {:flight-code 1260.0})

          data (str/join "\n" [data-headers line1 line2 line2 line2 line3])]
      (is (= [1220 1230 1240 1250 1260]
             (map :flight-code (main/parse-airline-data-table data))))))

  (testing "Flight codes don't appear in correct sequence"
    (let [line1 (generate-test-data-line {:flight-code 1220.0})
          line2 (generate-test-data-line {:flight-code nil})
          line3 (generate-test-data-line {:flight-code 84.0})

          data (str/join "\n" [data-headers line1 line2 line3 line2])]
      (is (= [1220 1230 84 94]
             (map :flight-code (main/parse-airline-data-table data)))))))


(deftest parse-and-re-stringify-test
  (testing "Happy path (example data)"
    (is (= (str "Airline Code;DelayTimes;FlightCodes;To;From\n"
                "Air Canada;[21 40];20015;Waterloo;Newyork\n"
                "Air France;[];20025;Montreal;Toronto\n"
                "Porter Airways;[60 22 87];20035;Calgary;Ottawa\n"
                "Air France;[78 66];20045;Ottawa;Vancouver\n"
                "Lufthansa;[12 33];20055;London;Montreal")
           (main/parse-and-re-stringify sample-data)))))
