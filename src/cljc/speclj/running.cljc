(ns speclj.running
  (:require [speclj.components :as components]
            [speclj.config :as config]
            [speclj.error :as error]
            [speclj.platform :as platform]
            [speclj.reporting :as reporting]
            [speclj.results :as results]
            [speclj.tags :as tags]))

(defn focus-mode? [component]
  (or (components/focused? component)
      (components/has-focus? component)
      (when-let [parent (components/parent component)]
        (recur parent))))

(defn can-run? [component]
  (or (components/focused? component)
      (components/has-focus? component)
      (not (focus-mode? component))))

(defn all-children [component]
  (if (components/is-description? component)
    (concat (components/characteristics component) (components/children component))
    []))

(defn focus-characteristics! [component]
  (components/focus! component)
  (doall (map components/focus! (components/characteristics component))))

(defn focus-children! [component]
  (components/focus! component)
  (doall (map focus-children! (components/children component))))

(defn enable-focus-mode! [component]
  (when-let [parent (components/parent component)]
    (components/enable-focus! parent)
    (recur parent)))

(defn track-focused-descriptions! [descriptions]
  (doseq [component descriptions]
    (when (components/focused? component)
      (enable-focus-mode! component)
      (focus-children! component)
      (focus-characteristics! component))))

(defn track-focused-characteristics! [characteristics]
  (->> (filter components/focused? characteristics)
       (run! enable-focus-mode!)))

(defn scan-for-focus! [description]
  (let [all (tree-seq some? all-children description)]
    (track-focused-descriptions! (filter components/is-description? all))
    (track-focused-characteristics! (filter components/is-characteristic? all))
    description))

(defn filter-focused [descriptions]
  (run! scan-for-focus! descriptions)
  (or (seq (filter focus-mode? descriptions)) descriptions))

(defn descriptions-with-namespaces [descriptions namespaces]
  (cond->> descriptions namespaces (filter #(namespaces (components/namespace-of %)))))

(defn- eval-components [components]
  (doseq [component components] ((components/body-of component))))

(defn nested-fns [base fns]
  (if (seq fns)
    (partial (first fns) (nested-fns base (rest fns)))
    base))

(defn- eval-characteristic [befores body afters]
  (eval-components befores)
  (try
    (body)
    (finally
      (eval-components afters))))

(defn- reset-withs [withs]
  (run! components/reset-with withs))

(defn- collect-components [getter description]
  (loop [description description components []]
    (if description
      (recur (components/parent description) (concat (getter description) components))
      components)))

(defn- report-result [result-constructor characteristic start-time reporters failure assertions]
  (let [present-args (filter identity [characteristic (platform/secs-since start-time) failure assertions])
        result       (apply result-constructor present-args)]
    (reporting/report-run result reporters)
    result))

(defn- do-characteristic [characteristic reporters]
  (binding [components/*assertions* (atom 0)]
    (let [description           (components/parent characteristic)
          befores               (collect-components components/befores description)
          afters                (collect-components components/afters description)
          core-body             (components/body-of characteristic)
          before-and-after-body (fn [] (eval-characteristic befores core-body afters))
          arounds               (collect-components components/arounds description)
          full-body             (nested-fns before-and-after-body (map components/body-of arounds))
          withs                 (collect-components components/withs description)
          start-time            (platform/current-time)]
      (try
        (full-body)
        (report-result results/pass-result characteristic start-time reporters nil @components/*assertions*)
        (catch #?(:cljd Object :clj java.lang.Throwable :cljs :default :default Exception) e
          (if (error/pending? e)
            (report-result results/pending-result characteristic start-time reporters e nil)
            (report-result results/fail-result characteristic start-time reporters e @components/*assertions*)))
        (finally
          (reset-withs withs))))))                          ;MDM - Possible clojure bug.  Inlining reset-withs results in compile error

(defn- do-characteristics [characteristics reporters]
  (doall
    (for [characteristic characteristics
          :when (can-run? characteristic)]
      (do-characteristic characteristic reporters))))

(declare do-description)

(defn- do-child-contexts [context results reporters]
  (loop [results  results
         children (components/children context)]
    (if (seq children)
      (recur (concat results (do-description (first children) reporters)) (rest children))
      (do
        (eval-components (components/after-alls context))
        results))))

(defn- results-for-context [context reporters]
  (if (tags/pass-tag-filter? (tags/tags-for context))
    (do-characteristics (components/characteristics context) reporters)
    []))

#?(:cljs
   (defn- with-withs-bound [description body]
     (let [withs (concat (components/withs description) (components/with-alls description))]
       (run! #((.-set-var! %) %) withs)
       (try
         (body)
         (finally
           (run! #((.-set-var! %) nil) withs)))))

   :cljd
   (defn- with-withs-bound [_description body] (body))

   :default
   (defn- with-withs-bound [description body]
     (let [withs                (concat (components/withs description) (components/with-alls description))
           ns                   (the-ns (symbol (components/namespace-of description)))
           with-mappings        (reduce #(assoc %1 (ns-resolve ns (components/name-of %2)) %2) {} withs)
           with-and-ns-mappings (assoc with-mappings #'*ns* ns)]
       (with-bindings* with-and-ns-mappings body)))
   )

(defn- nested-results-for-context [description reporters]
  (let [results (results-for-context description reporters)]
    (do-child-contexts description results reporters)))

(defn- with-around-alls [description run-characteristics-fn]
  ((nested-fns run-characteristics-fn
               (map components/body-of (components/around-alls description)))))

(defn do-description [description reporters]
  (when (can-run? description)
    (let [tag-sets (tags/tag-sets-for description)]
      (when (some tags/pass-tag-filter? tag-sets)
        (binding [components/*assertions* (atom 0)]
          (reporting/report-description* reporters description)
          (with-withs-bound description
            (fn []
              (eval-components (components/before-alls description))

              (try
                (with-around-alls
                  description
                  (partial nested-results-for-context description reporters))

                (finally
                  (reset-withs (components/with-alls description)))))))))))

(defprotocol Runner
  (run-directories [this directories reporters])
  (submit-description [this description])
  (-filter-descriptions [this namespaces])
  (-get-descriptions [this])
  (run-description [this description reporters])
  (run-and-report [this reporters])
  (run-results [this])
  (submit-result [this result]))

(defn process-compile-error [runner e]
  (let [error-result (results/error-result e)]
    (submit-result runner error-result)
    (reporting/report-run error-result (config/active-reporters))))

(defn ^:export filter-descriptions
  "Protocol method defined as function for JavaScript interoperability"
  [runner namespaces]
  (->> namespaces
       #?(:cljs js->clj)
       (-filter-descriptions runner)))

(defn ^:export get-descriptions [runner]
  (-> runner -get-descriptions into-array))
