(ns speclj.run.vigilant
  (:require [clojure.java.io :as io]
            [fresh.core :refer [clj-files-in make-fresh ns-to-file]]
            [speclj.config :as config]
            [speclj.platform :refer [current-time endl enter-pressed? format-seconds secs-since]]
            [speclj.reporting :as reporting]
            [speclj.results :as results]
            [speclj.running :as running])
  (:import (java.util.concurrent ScheduledThreadPoolExecutor TimeUnit)))

(def start-time (atom 0))

(defn- ns-for-results [results]
  (set (map #(str (.ns @(.. % characteristic parent))) results)))

(defn- report-update [report]
  (let [reporters (config/active-reporters)
        reloads   (:reloaded report)]
    (when (seq reloads)
      (reporting/report-message* reporters (str endl "----- " (str (java.util.Date.) " -----")))
      (reporting/report-message* reporters (str "took " (format-seconds (secs-since @start-time)) " to determine file statuses."))
      (reporting/report-message* reporters "reloading files:")
      (doseq [file reloads] (reporting/report-message* reporters (str "  " (.getCanonicalPath file))))))
  true)

(defn- reload-files [runner current-results]
  (let [previous-failed-files (map ns-to-file (ns-for-results @(.previous-failed runner)))
        files-to-reload       (set (concat previous-failed-files current-results))]
    (swap! (.file-listing runner) #(apply dissoc % previous-failed-files))
    (make-fresh (.file-listing runner) files-to-reload report-update)))

(defn- reload-report [runner report]
  (let [reloads (:reloaded report)]
    (when (seq reloads)
      (reload-files runner reloads)))
  false)

(defn- tick [configuration]
  (with-bindings configuration
    (let [runner    (config/active-runner)
          reporters (config/active-reporters)]
      (try
        (reset! start-time (current-time))
        (make-fresh (.file-listing runner) (set (apply clj-files-in @(.directories runner))) (partial reload-report runner))
        (when (seq @(.descriptions runner))
          (reset! (.previous-failed runner) (:fail (results/categorize (seq @(.results runner)))))
          (running/run-and-report runner reporters))
        (catch java.lang.Throwable e
          (running/process-compile-error runner e)
          (reporting/report-runs* reporters @(.results runner))))
      (reset! (.descriptions runner) [])
      (reset! (.results runner) []))))

(defn- reset-runner [runner]
  (reset! (.previous-failed runner) [])
  (reset! (.results runner) [])
  (reset! (.file-listing runner) {}))

(defn- listen-for-rerun [configuration]
  (with-bindings configuration
    (when (enter-pressed?)
      (reset-runner (config/active-runner)))))

(deftype VigilantRunner [file-listing results previous-failed directories descriptions]
  running/Runner
  (run-directories [this directories _reporters]
    (let [scheduler     (ScheduledThreadPoolExecutor. 1)
          configuration (config/config-bindings)
          runnable      (fn [] (tick configuration))
          dir-files     (map io/file directories)]
      (reset! (.directories this) dir-files)
      (.scheduleWithFixedDelay scheduler runnable 0 500 TimeUnit/MILLISECONDS)
      (.scheduleWithFixedDelay scheduler (fn [] (listen-for-rerun configuration)) 0 500 TimeUnit/MILLISECONDS)
      (.awaitTermination scheduler Long/MAX_VALUE TimeUnit/SECONDS)
      0))

  (submit-description [_this description]
    (swap! descriptions conj description))

  (-get-descriptions [_this] @descriptions)

  (-filter-descriptions [_this namespaces]
    (swap! descriptions running/descriptions-with-namespaces namespaces))

  (run-description [_this description reporters]
    (->> (running/do-description description reporters)
         (swap! results into)))

  (run-and-report [this reporters]
    (doseq [description (running/filter-focused @descriptions)]
      (running/run-description this description reporters))
    (reporting/report-runs* reporters @results)))

(defn new-vigilant-runner []
  (VigilantRunner. (atom {}) (atom []) (atom []) (atom nil) (atom [])))
