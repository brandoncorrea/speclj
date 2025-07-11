(ns speclj.config
  (:require [clojure.string :as str]
            [speclj.platform :as platform]))

(def ^:dynamic *parent-description*)
(def ^:dynamic *profile?*)
(def ^:dynamic *reporters*)

(def default-reporters (atom nil))

(defn parent-description-bound? []
  #?(:cljs *parent-description* :cljd *parent-description* :default (bound? #'*parent-description*)))

(defn active-reporters []
  (if #?(:cljs *reporters* :cljd *reporters* :default (bound? #'*reporters*))
    *reporters*
    (if-let [reporters @default-reporters]
      reporters
      (throw (new #?(:cljs js/Error :default Exception) "*reporters* is unbound and no default value has been provided")))))

(def ^:dynamic *runner*)
(def default-runner (atom nil))
(def default-runner-fn (atom nil))

(defn ^:export active-runner []
  (if #?(:cljs *runner* :cljd *runner* :default (bound? #'*runner*))
    *runner*
    (if-let [runner @default-runner]
      runner
      (throw (new #?(:cljs js/Error :default Exception)
                  "*runner* is unbound and no default value has been provided")))))

(def ^:dynamic *specs*)
(def ^:dynamic *omit-pending?* false)
(def ^:dynamic *color?* false)
(def ^:dynamic *full-stack-trace?* false)
(def ^:dynamic *tag-filter* {:include #{} :exclude #{}})

(def default-config
  {:specs        ["spec"]
   :runner       "standard"
   :reporters    ["progress"]
   :tags         []
   :omit-pending false})

#?(:cljs
   (defn config-bindings [] (throw "Not Supported in ClojureScript"))

   :cljd
   (defn config-bindings [] (throw (new Exception "Not Supported in ClojureDart")))

   :default
   (defn config-bindings
     "Returns a map of vars to values for all the ear-muffed vars in the speclj.config namespace.
     Can be used in (with-bindings ...) call to load a configuration state"
     []
     (let [ns              (the-ns 'speclj.config)
           all-vars        (dissoc (ns-interns ns) '*parent-description*)
           non-config-keys (remove #(str/starts-with? (name %) "*") (keys all-vars))
           config-vars     (apply dissoc all-vars non-config-keys)]
       (reduce #(assoc %1 %2 (deref %2)) {} (vals config-vars)))))

(defn load-runner [name]
  (try
    (platform/dynamically-invoke (str "speclj.run." name) (str "new-" name "-runner"))
    (catch #?(:cljs :default :default Exception) e
      (throw (new #?(:cljs js/Error :default Exception) (str "Failed to load runner: " name) #?@(:cljd [] :default [e]))))))

(defn- load-reporter-by-name [name]
  (try
    (platform/dynamically-invoke (str "speclj.report." name) (str "new-" name "-reporter"))
    (catch #?(:cljs :default :default Exception) e
      (throw (new #?(:cljs js/Error :default Exception) (str "Failed to load reporter: " name) #?@(:cljd [] :default [e]))))))

(defn- load-reporter-by-name? [name-or-object]
  #?(:default (string? name-or-object)
     :clj     (->> name-or-object
                   (instance? (Class/forName "speclj.reporting.Reporter"))
                   not)))

(defn load-reporter [name-or-object]
  (cond-> name-or-object
          (load-reporter-by-name? name-or-object)
          load-reporter-by-name))

(defn- parse-tag [tag]
  (let [tag (name tag)]
    (if (str/starts-with? tag "~")
      [:excludes (str/replace tag #"^~" "")]
      [:includes tag])))

(defn- with-tag [tag-filter tag]
  (let [[flag value] (parse-tag tag)]
    (update tag-filter flag conj (keyword value))))

(defn parse-tags [tags]
  (reduce with-tag {:includes #{} :excludes #{}} tags))

#?(:cljs
   (defn config-mappings [_] (throw "Not Supported in ClojureScript"))

   :cljd
   (defn config-mappings [_] (throw (Exception. "Not Supported in ClojureDart")))

   :default
   (defn config-mappings [config]
     {#'*runner*            (if (:runner config) (load-runner (:runner config)) (active-runner))
      #'*reporters*         (if (:reporters config) (map load-reporter (:reporters config)) (active-reporters))
      #'*specs*             (:specs config)
      #'*color?*            (:color config)
      #'*profile?*          (:profile config)
      #'*omit-pending?*     (:omit-pending config)
      #'*full-stack-trace?* (some? (:stacktrace config))
      #'*tag-filter*        (parse-tags (:tags config))})

   )

(defn with-config
  "Runs the given function with all the configurations set.  Useful in cljs because config-mappings can't be used."
  [config action]
  (binding [*runner*            (if (:runner config) (load-runner (:runner config)) (active-runner))
            *reporters*         (if (:reporters config) (mapv load-reporter (:reporters config)) (active-reporters))
            *specs*             (:specs config)
            *color?*            (:color config)
            *profile?*          (:profile config)
            *omit-pending?*     (:omit-pending config)
            *full-stack-trace?* (some? (:stacktrace config))
            *tag-filter*        (parse-tags (:tags config))]
    (action)))
