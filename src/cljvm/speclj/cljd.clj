(ns speclj.cljd
  (:require [cljd.build :as build]))

(defn test-cli [& {:keys [namespaces]}]
  (when (build/compile-cli :namespaces namespaces)
    (newline)
    (println "Running tests...")
    (let [bin (some-> build/*deps* :cljd/opts :kind name)]
      (build/exec {:in nil} bin "test"))))

(defn -main []
  (test-cli :namespaces ['speclj.args-spec]))
