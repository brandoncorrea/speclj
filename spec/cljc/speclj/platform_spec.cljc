(ns speclj.platform-spec
  (:require #?@(:cljs [] :default [[clojure.tools.namespace.find :as find]])
            [speclj.core #?(:cljs :refer-macros :default :refer) [describe should-be it should-not-be should-contain should= should-throw]]
            [speclj.platform :as sut #?(:cljs :refer-macros :default :refer) [if-cljs try-catch-anything]]
            [speclj.run.standard :as standard]))

(defmacro which-env []
  (if-cljs :cljs :clj))

(defn ->stack-element [class-name]
  #?(:clj     (StackTraceElement. class-name "foo_method" (str class-name ".clj") 123)
     :default class-name))

(describe "platform-specific bits"
  #?(:cljs
     (it "javascript object stays pristine"
       (should= {} (js->clj (js-obj)))))

  (it "line-separator"
    (let [separator #?(:clj  (System/getProperty "line.separator")
                       :cljs "\n"
                       :cljr Environment/NewLine)]
      (should-be string? sut/endl)
      (should= separator sut/endl)))

  (it "file-separator"
    (let [separator #?(:clj  (System/getProperty "file.separator")
                       :cljs "/"
                       :cljr (str System.IO.Path/DirectorySeparatorChar))]
      (should-be string? sut/file-separator)
      (should= separator sut/file-separator)))

  (it "source-file-regex"
    (let [re #?(:bb   ".*\\.(cljc|clj|bb)"
                :clj  ".*\\.clj(c)?"
                :cljs "/.*\\.clj(c|s)/"
                :cljr ".*\\.clj(c|r)?")]
      (should-be sut/re? sut/source-file-regex)
      (should= re (str sut/source-file-regex))))

  (it "type-name"
    (should= #?(:clj  "java.lang.Exception"
                :cljr "System.Exception"
                :cljs "Error")
             (sut/type-name #?(:cljs js/Error :default Exception)))
    (should= #?(:clj  "java.lang.Object"
                :cljr "System.Object"
                :cljs "Object")
             (sut/type-name #?(:cljs js/Object :default Object))))

  (it "failure-source"
    (let [ex (ex-info "the failure" {})]
      #?(:cljr (try (throw ex) (catch Exception _)))
      ; Babashka doesn't know where runtime exceptions occur
      #?(:bb   (should-contain #"^sci/lang/Var.clj:\d+$" (sut/failure-source-str ex))
         :clj  (should-contain #"^speclj/platform_spec.clj:\d+$" (sut/failure-source-str ex))
         :cljr (should= "speclj/platform_spec" (sut/failure-source-str ex)))))

  (it "elide-level?"
    (#?(:cljs should-not-be :default should-be) sut/elide-level? (->stack-element "clojure.core.blah"))
    (#?(:cljs should-not-be :default should-be) sut/elide-level? (->stack-element "speclj.core.blah"))
    (#?(:bb should-be :default should-not-be) sut/elide-level? (->stack-element "babashka.foo"))
    (#?(:bb should-be :default should-not-be) sut/elide-level? (->stack-element "sci.foo"))
    (#?(:bb should-be :default should-not-be) sut/elide-level? (->stack-element "edamame.foo"))
    (should-not-be sut/elide-level? (->stack-element "specljs"))
    (should-not-be sut/elide-level? (->stack-element "clojures")))

  #?(:cljs (list)
     :default
     (it "get-bytes"
       (should= [102 111 111] (sut/get-bytes "foo"))))

  #?(:bb
     (it "find-platform"
       (should= sut/find-platform {:read-opts {:read-cond :allow
                                               :features #{:bb :clj}}
                                   :extensions [".bb" ".clj" ".cljc"]}))
     :clj
     (it "find-platform"
       (should= sut/find-platform find/clj))
     :cljr
     (it "find-platform"
       (should= sut/find-platform find/cljr)))

  (it "if-cljs conditionally compiles a macro"
    (should= #?(:cljs :cljs :default :clj) (which-env)))

  (describe "try-catch-anything"
    (let [throwable #?(:clj (Throwable. "welp")
                       :cljs "welp"
                       :cljr (Exception. "welp"))]
      (it "catches anything"
        (try-catch-anything
          (throw throwable)
          (catch e
                 (should= e throwable))))

      (it "throws if the last form is not a catch"
        (should-throw
          (try-catch-anything
            :nope)))

      (it "throws if the binding is not a symbol"
        (should-throw
          (try-catch-anything
            :yep
            (catch :nope 'whatever))))))
  )

(standard/run-specs)
