(ns speclj.report.progress
  (:require [clojure.string :as str]
            [speclj.components :as components]
            [speclj.config :as config]
            [speclj.error :as error]
            [speclj.platform :as platform]
            [speclj.reporting :as reporting]
            [speclj.results :as results]))

(defn full-name [characteristic]
  (loop [context (components/parent characteristic)
         name    (components/name-of characteristic)]
    (if context
      (recur (components/parent context) (str (components/name-of context) " " name))
      name)))

(defn print-failure [id result]
  (let [characteristic (results/characteristic result)
        failure        (results/failure result)]
    (println)
    (println (reporting/indent 1 id ") " (full-name characteristic)))
    (println (reporting/red (reporting/indent 2.5 (ex-message failure))))
    (if (error/failure? failure)
      (println (reporting/grey (reporting/indent 2.5 (platform/failure-source-str failure))))
      (println (reporting/grey (reporting/indent 2.5 (reporting/stack-trace-str failure)))))))

(defn print-failures [failures]
  (when (seq failures)
    (println)
    (println "Failures:"))
  (dotimes [i (count failures)]
    (print-failure (inc i) (nth failures i))))

(defn print-pendings [pending-results]
  (when-not config/*omit-pending?*
    (when (seq pending-results)
      (println)
      (println "Pending:"))
    (doseq [result pending-results]
      (println)
      (println (reporting/yellow (str "  " (full-name (results/characteristic result)))))
      (println (reporting/grey (str "    ; " (ex-message (results/exception result)))))
      (println (reporting/grey (str "    ; " (platform/failure-source-str (results/exception result))))))))

(defn print-errors [error-results]
  (when (seq error-results)
    (println)
    (println "Errors:"))
  (doseq [[number result] (partition 2 (interleave (iterate inc 1) error-results))]
    (println)
    (println (reporting/indent 1 number ") " (reporting/red (str (results/exception result)))))
    (println (reporting/grey (reporting/indent 2.5 (reporting/stack-trace-str (results/exception result))))))
  (platform/flush))

(defn- print-duration [results]
  (println)
  (println "Finished in" (platform/format-seconds (reporting/tally-time results)) "seconds"))

(defn color-fn-for [result-map]
  (cond
    (not= 0 (count (concat (:fail result-map) (:error result-map)))) reporting/red
    (not= 0 (count (:pending result-map))) reporting/yellow
    :else reporting/green))

(defn- apply-pending-tally [report tally]
  (if (pos? (:pending tally))
    (conj report (str (:pending tally) " pending"))
    report))

(defn- apply-error-tally [report tally]
  (if (pos? (:error tally))
    (conj report (str (:error tally) " errors"))
    report))

(defn describe-counts-for [result-map]
  (let [tally            (zipmap (keys result-map) (map count (vals result-map)))
        always-on-counts [(str (apply + (vals tally)) " examples")
                          (str (:fail tally) " failures")
                          (str (reporting/tally-assertions (concat (:pass result-map) (:fail result-map))) " assertions")]]
    (str/join ", "
              (-> always-on-counts
                  (apply-pending-tally tally)
                  (apply-error-tally tally)))))

(defn- print-tally [result-map]
  (let [color-fn (color-fn-for result-map)]
    (println (color-fn (describe-counts-for result-map)))))

(defn print-summary [results]
  (let [result-map (results/categorize results)]
    (print-failures (:fail result-map))
    (print-pendings (:pending result-map))
    (print-errors (:error result-map))
    (print-duration results)
    (print-tally result-map)))

(deftype ProgressReporter []
  reporting/Reporter
  (report-message [_this message]
    (println message) (platform/flush))
  (report-description [_this _description])
  (report-pass [_this _result]
    (print (reporting/green ".")) (platform/flush))
  (report-pending [_this _result]
    (print (reporting/yellow "*")) (platform/flush))
  (report-fail [_this _result]
    (print (reporting/red "F")) (platform/flush))
  (report-error [_this _result]
    (print (reporting/red "E")) (platform/flush))
  (report-runs [_this results]
    (println)
    (print-summary results)))

(defn ^:export new-progress-reporter []
  (ProgressReporter.))

(reset! config/default-reporters [(new-progress-reporter)])
