(ns hk-passenger-io.main
  (:require
    [clojure.data.csv :as csv]
    [clojure.java.io :as io]
    [clojure.string :as string]
    [org.httpkit.client :as http]
    [org.httpkit.sni-client]
    [promesa.core :as p]
    [reaver :as r :refer [parse extract-from text attr chain]])
  (:import
    (java.time
      LocalDate)
    (java.time.format
      DateTimeFormatter)
    (java.time.temporal
      ChronoUnit)))


(def dir "data/")

(def filename "hk_passenger_io_")

(def first-date (LocalDate/of 2020 1 24))

(def parse-date-formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd"))

(def date-formatter (DateTimeFormatter/ofPattern "yyyyMMdd"))


(.format date-formatter first-date)


(defn parse-int
  [num-str]
  (when num-str
    (Integer/parseInt (string/replace num-str "," ""))))


(defn parse-html
  [body]
  (extract-from
    (parse body)
    "table.table-stat tr[class!=txt_bold]:gt(4)"
    [:control_point :arrive_resident :arrive_mainland :arrive_others :arrive_total :departure_resident :departure_mainland :departure_others :departure_total]
    "td:eq(0)" text
    "td:eq(1)" (chain text parse-int)
    "td:eq(2)" (chain text parse-int)
    "td:eq(3)" (chain text parse-int)
    "td:eq(4)" (chain text parse-int)
    "td:eq(5)" (chain text parse-int)
    "td:eq(6)" (chain text parse-int)
    "td:eq(7)" (chain text parse-int)
    "td:eq(8)" (chain text parse-int)
    "td:eq(9)" (chain text parse-int)))


(defn fetch-rows
  [date]
  (prn 'fetch date)
  (let [url (str "https://www.immd.gov.hk/eng/stat_"
                 (.format date-formatter date)
                 ".html")
        p (p/deferred)
        _ (http/get url
                    (fn [resp] (p/resolve! p (:body resp)))
                    (fn [err]
                      (prn 'failed url)
                      (p/reject! p nil)))]
    (p/chain' p
              parse-html
              (fn [rows]
                (map #(assoc % :date date) rows)))))


(def map->vec
  (juxt :date :control_point :arrive_resident :arrive_mainland :arrive_others :arrive_total :departure_resident :departure_mainland :departure_others :departure_total))


(defn fetch-all
  [^LocalDate from-date ^LocalDate to-date]
  (let [n (.between ChronoUnit/DAYS from-date to-date)
        xf (comp (map #(.minusDays to-date %))
                 (map fetch-rows))]
    (p/chain' (p/all (into [] xf (range n)))
              (fn [all] (apply concat all))
              #(sort-by :date %))))


(defn write-csv!
  [rows fname]
  (with-open [writer (io/writer fname :append true)]
    (csv/write-csv writer
                   rows)))


(defn get-last-record-date
  [fname]
  (when (.exists (io/file fname))
    (with-open [rdr (io/reader (io/file fname))]
      (->> rdr
           line-seq
           last
           (csv/read-csv)
           ffirst
           (.parse parse-date-formatter)
           (LocalDate/from)))))


(defn -main
  [& args]
  (let [fname (str dir filename (.getYear (LocalDate/now)) ".csv")]
    (-> (fetch-all (or (get-last-record-date fname)
                       first-date)
                   (LocalDate/now))
        (p/chain' #(map map->vec %)
                  #(write-csv! % fname))
        deref)))
