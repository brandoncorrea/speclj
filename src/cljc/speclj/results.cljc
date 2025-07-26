(ns speclj.results)

(defn pass-result [characteristic seconds assertions]
  {:kind           :pass
   :characteristic characteristic
   :seconds        seconds
   :assertions     assertions})

(defn fail-result [characteristic seconds failure assertions]
  {:kind           :fail
   :characteristic characteristic
   :seconds        seconds
   :failure        failure
   :assertions     assertions})

(defn pending-result [characteristic seconds exception]
  {:kind           :pending
   :characteristic characteristic
   :seconds        seconds
   :exception      exception})

(defn error-result [exception]
  {:kind      :error
   :seconds   0
   :exception exception})

(defn pass? [result] (= :pass (:kind result)))
(defn fail? [result] (= :fail (:kind result)))
(defn pending? [result] (= :pending (:kind result)))
(defn error? [result] (= :error (:kind result)))
(defn failure? [result] (or (fail? result) (error? result)))
(defn fail-count [results] (count (filter failure? results)))
(defn characteristic [result] (:characteristic result))
(defn seconds [result] (:seconds result))
(defn exception [result] (:exception result))
(defn failure [result] (:failure result))
(defn assertions [result] (:assertions result))

(defn- tally-result [tally result]
  (update tally (:kind result) conj result))

(defn categorize [results]
  (reduce tally-result {:pending [] :fail [] :pass [] :error []} results))
