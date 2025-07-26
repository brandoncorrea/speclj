(ns speclj.tags
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [speclj.components :as components]
            [speclj.config :as config]))

(defn pass-includes? [includes tags]
  (or (empty? includes)
      (= includes (set/intersection includes (set tags)))))

(defn pass-excludes? [excludes tags]
  (or (empty? excludes)
      (not-any? #(contains? excludes %) tags)))

(defn pass-tag-filter?
  ([tags] (pass-tag-filter? config/*tag-filter* tags))
  ([filter tags]
   (and
     (pass-includes? (:includes filter) tags)
     (pass-excludes? (:excludes filter) tags))))

(defn tags-for [context]
  (if context
    (set/union (tags-for (components/parent context))
               (components/tags context))
    #{}))

(defn tag-sets-for [context]
  (let [context-seq (tree-seq some? components/children context)]
    (map tags-for context-seq)))

(defn describe-filter
  ([] (describe-filter config/*tag-filter*))
  ([filter]
   (let [includes (seq (map name (:includes filter)))
         excludes (seq (map name (:excludes filter)))]
     (when (or includes excludes)
       (str "Filtering tags."
            (when includes (str " Including: " (str/join ", " includes) "."))
            (when excludes (str " Excluding: " (str/join ", " excludes) ".")))))))
