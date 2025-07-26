(ns speclj.report.documentation
  (:require [speclj.components :as components]
            [speclj.config :as config]
            [speclj.platform :as platform]
            [speclj.report.progress :as progress]
            [speclj.reporting :as reporting]
            [speclj.results :as results]))

(defn level-of [component]
  (loop [description (components/parent component)
         level       0]
    (if description
      (recur (components/parent description) (inc level))
      level)))

(defn maybe-focused [text component]
  (cond-> text
          (components/focused? component)
          (str " " (reporting/yellow "[FOCUS]"))))

(defn- maybe-profile
  ([text]
   (cond->> text
            config/*profile?*
            (str "           ")))
  ([text result]
   (cond->> text
            config/*profile?*
            (str (reporting/yellow (str "[" (platform/format-seconds (results/seconds result)) "s] "))))))

(deftype DocumentationReporter []
  reporting/Reporter

  (report-message [_this message]
    (println message)
    (platform/flush))

  (report-description [_this description]
    (let [level (level-of description)]
      (when (zero? level) (println))
      (let [output (-> (reporting/indent level (components/name-of description))
                       (maybe-focused description)
                       maybe-profile)]
        (println output)
        (platform/flush))))

  (report-pass [_this result]
    (let [characteristic (results/characteristic result)
          level          (level-of characteristic)
          output         (-> (reporting/indent (dec level) "- " (components/name-of characteristic))
                             reporting/green
                             (maybe-focused characteristic)
                             (maybe-profile result))]
      (println output)
      (platform/flush)))

  (report-pending [_this result]
    (let [characteristic (results/characteristic result)
          level          (level-of characteristic)
          output         (-> (reporting/indent (dec level) "- " (components/name-of characteristic) " (PENDING: " (ex-message (results/exception result)) ")")
                             reporting/yellow
                             (maybe-profile result))]
      (println output)
      (platform/flush)))

  (report-fail [_this result]
    (let [characteristic (results/characteristic result)
          level          (level-of characteristic)
          output         (-> (reporting/indent (dec level) "- " (components/name-of characteristic) " (FAILED)")
                             reporting/red
                             (maybe-focused characteristic)
                             (maybe-profile result))]
      (println output)
      (platform/flush)))

  (report-error [_this result]
    (println (reporting/red (#?(:cljr .ToString :default .toString) (results/exception result)))))

  (report-runs [_this results]
    (progress/print-summary results)))

(defn ^:export new-documentation-reporter []
  (DocumentationReporter.))
