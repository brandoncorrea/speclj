(ns speclj.args-helper
  #?(:cljs (:require-macros [speclj.args-helper :refer [should-have-parse-error should-have-leftover created-string-should= option-string-should= parameter-string-should=]]))
  (:require [speclj.core #?(:cljs :refer-macros :default :refer) [context should-throw should-not-throw should-not-contain should-contain it should= should-be should-not-be-nil]]
            [speclj.args :as sut]
            [clojure.string :as str]))

#?(:cljs    (do)
   :default (defmacro should-have-parse-error [spec message & args]
              `(let [result# (sut/parse ~spec [~@args])
                     errors# (:*errors result#)]
                 (should-not-be-nil errors#)
                 (should-contain ~message errors#))))

#?(:cljs    (do)
   :default (defmacro should-have-leftover [spec leftovers args]
              `(should= ~leftovers (:*leftover (sut/parse ~spec ~args)))))

#?(:cljs    (do)
   :default (defmacro created-string-should= [f spec & lines]
              `(should= (str (str/join "\n" [~@lines]) "\n") (~f ~spec))))

#?(:cljs    (do)
   :default (defmacro option-string-should= [spec & lines]
              `(created-string-should= sut/options-string ~spec ~@lines)))

#?(:cljs    (do)
   :default (defmacro parameter-string-should= [spec & lines]
              `(created-string-should= sut/parameters-string ~spec ~@lines)))

(defn test-arguments [constructor]
  (context "Argument Parsing"

    (it "parses nothing"
      (let [spec (constructor)]
        (should-be empty? (sut/parse spec []))))

    (it "unexpected parameter"
      (let [spec (constructor)]
        (should-have-parse-error spec "Unexpected parameter: foo" "foo")
        (should-have-parse-error spec "Unexpected parameter: bar" "bar")))

    (it "one parameter"
      (let [spec (-> (constructor)
                     (sut/add-parameter "foo" "Some Description"))]
        (should= {:foo "bar"} (sut/parse spec ["bar"]))
        (should= {:foo "fizz"} (sut/parse spec ["fizz"]))))

    (it "missing parameter"
      (let [spec (-> (constructor)
                     (sut/add-parameter "foo" "Some Description"))]
        (should-have-parse-error spec "Missing parameter: foo")))

    (it "two parameters"
      (let [result (-> (constructor)
                       (sut/add-parameter "foo" "Some Description")
                       (sut/add-parameter "bar" "Some Description")
                       (sut/parse ["fizz" "bang"]))]
        (should= {:foo "fizz" :bar "bang"} result)))

    (it "missing one of two parameters"
      (let [spec (-> (constructor)
                     (sut/add-parameter "foo" "Some Description")
                     (sut/add-parameter "bar" "Some Description"))]
        (should-have-parse-error spec "Missing parameter: foo")
        (should-have-parse-error spec "Missing parameter: bar" "fizz")))

    (it "optional parameter"
      (let [spec (-> (constructor)
                     (sut/add-optional-parameter "foo" "Some Description"))]
        (should= {} (sut/parse spec []))
        (should= {:foo "fizz"} (sut/parse spec ["fizz"]))))

    (it "one switch option"
      (let [spec (-> (constructor)
                     (sut/add-switch-option "m" "my-option" "my test option"))]
        (should-not-contain :my-option (sut/parse spec []))
        (should= {:my-option "on"} (sut/parse spec ["-m"]))
        (should= {:my-option "on"} (sut/parse spec ["--my-option"]))))

    (it "two switch options"
      (let [spec      (-> (constructor)
                          (sut/add-switch-option "a" "a-option" "Option A")
                          (sut/add-switch-option "b" "b-option" "Option B"))
            parsed-a  (sut/parse spec ["-a"])
            parsed-b  (sut/parse spec ["--b-option"])
            parsed-ab (sut/parse spec ["--a-option" "-b"])]
        (should= {} (sut/parse spec []))
        (should-contain :a-option parsed-a)
        (should-not-contain :b-option parsed-a)
        (should-not-contain :a-option parsed-b)
        (should-contain :b-option parsed-b)
        (should-contain :a-option parsed-ab)
        (should-contain :b-option parsed-ab)))

    (it "option names are required"
      (let [spec (constructor)]
        (should-throw #?(:clj RuntimeException :cljs js/Error :cljr SystemException) "Options require a shortName and fullName"
          (sut/add-switch-option spec "a" nil nil))
        (should-throw #?(:clj RuntimeException :cljs js/Error :cljr SystemException) "Options require a shortName and fullName"
          (sut/add-switch-option spec nil "a-option" nil))
        (should-not-throw (sut/add-switch-option spec "a" "a-option" nil))))

    (it "unrecognized option"
      (let [spec (constructor)]
        (should-have-parse-error spec "Unrecognized option: -a" "-a")
        (should-have-parse-error spec "Unrecognized option: --a-option" "--a-option")))

    (it "one value option"
      (let [spec (-> (constructor)
                     (sut/add-value-option "a" "a-option" "value" "Option A"))]
        (should= {:a-option "value"} (sut/parse spec ["-a" "value"]))
        (should= {:a-option "value"} (sut/parse spec ["--a-option=value"]))))

    (it "missing option value"
      (let [spec (-> (constructor)
                     (sut/add-value-option "a" "a-option" "value" "Option A"))]
        (should-have-parse-error spec "Missing value for option: a" "-a")
        (should-have-parse-error spec "Missing value for option: a-option" "--a-option")))

    (it "missing option value when followed by option"
      (let [spec (-> (constructor)
                     (sut/add-value-option "a" "a-option" "value" "Option A")
                     (sut/add-switch-option "b" "b-option" "Option B"))]
        (should-have-parse-error spec "Missing value for option: a" "-a" "-b")
        (should-have-parse-error spec "Missing value for option: a" "-a" "--b-option")))

    (it "parameter with switch option"
      (let [spec (-> (constructor)
                     (sut/add-parameter "param" "Some Description")
                     (sut/add-switch-option "a" "a-option" "Option a"))]
        (should-have-parse-error spec "Missing parameter: param")
        (should-have-parse-error spec "Missing parameter: param" "-a")
        (should-have-parse-error spec "Missing parameter: param" "--a-option")
        (should= {:a-option "on" :param "blah"} (sut/parse spec ["-a" "blah"]))
        (should= {:a-option "on" :param "blah"} (sut/parse spec ["--a-option" "blah"]))))

    (it "parameter with value option"
      (let [spec (-> (constructor)
                     (sut/add-parameter "param" "Some Description")
                     (sut/add-value-option "a" "a-option" "value" "Option A"))]
        (should-have-parse-error spec "Missing parameter: param")
        (should-have-parse-error spec "Missing value for option: a" "-a")
        (should-have-parse-error spec "Missing parameter: param" "-a" "foo")
        (should-have-parse-error spec "Missing parameter: param" "--a-option=foo")
        (should= {:a-option "foo" :param "bar"} (sut/parse spec ["-a" "foo" "bar"]))
        (should= {:a-option "foo" :param "bar"} (sut/parse spec ["--a-option=foo" "bar"]))))

    (it "parameter options are parsable in long form without equals sign"
      (let [result (-> (constructor)
                       (sut/add-parameter "param" "Some Description")
                       (sut/add-value-option "a" "a-option" "value" "Option A")
                       (sut/parse ["--a-option" "foo" "bar"]))]
        (should= {:a-option "foo" :param "bar"} result)))

    (it "remaining args"
      (let [blank-spec   (constructor)
            param-spec   (-> (constructor)
                             (sut/add-parameter "param" "Some Description"))
            param-a-spec (-> (constructor)
                             (sut/add-parameter "param" "Some Description")
                             (sut/add-switch-option "a" "a-option" "Option A"))]
        (should-have-leftover blank-spec ["foo"] ["foo"])
        (should-have-leftover param-spec ["bar"] ["foo" "bar"])
        (should-have-leftover param-a-spec ["bar"] ["-a" "foo" "bar"])
        (should-have-leftover param-a-spec ["-z" "bar"] ["-z" "foo" "bar"])))

    (it "remaining args with value option"
      (let [spec (-> (constructor)
                     (sut/add-parameter "param" "Some Description")
                     (sut/add-value-option "a" "a-option" "value" "Option A"))]
        (should-have-leftover spec ["-z"] ["-z"])
        (should-have-leftover spec ["-z" "bar"] ["-z" "foo" "bar"])
        (should-have-leftover spec ["fizz"] ["-a" "foo" "bar" "fizz"])))

    (it "can parse options mixed in with parameters"
      (let [result (-> (constructor)
                       (sut/add-parameter "param1" "Some Description")
                       (sut/add-parameter "param2" "Some Description")
                       (sut/add-switch-option "a" "a-switch" "Switch A")
                       (sut/add-value-option "b" "b-option" "B" "Option B")
                       (sut/add-value-option "c" "c-option" "C" "Option C")
                       (sut/parse ["-a" "one" "--b-option=two" "three" "--c-option" "four" "five"]))]
        (should= "on" (:a-switch result))
        (should= "one" (:param1 result))
        (should= "two" (:b-option result))
        (should= "three" (:param2 result))
        (should= "four" (:c-option result))))

    (it "multi parameters"
      (let [spec (-> (constructor)
                     (sut/add-multi-parameter "colors" "Any number of colors"))]
        (should= {:colors ["red" "orange" "yellow"]} (sut/parse spec ["red" "orange" "yellow"]))
        (should= {} (sut/parse spec []))
        (should= {:colors ["red"]} (sut/parse spec ["red"]))))

    (it "multi options"
      (let [spec (-> (constructor)
                     (sut/add-multi-option "c" "color" "COLOR" "Some colors"))]
        (should= {} (sut/parse spec []))
        (should= {:color ["red"]} (sut/parse spec ["-c" "red"]))
        (should= {:color ["red" "orange" "yellow"]} (sut/parse spec ["-c" "red" "--color" "orange" "--color=yellow"]))))

    (context "arg-string"

      (it "empty"
        (should= "" (sut/arg-string (constructor))))

      (it "one parameter"
        (let [spec (-> (constructor)
                       (sut/add-parameter "param" "Some Description"))]
          (should= "<param>" (sut/arg-string spec))))

      (it "parameter and switch"
        (let [spec (-> (constructor)
                       (sut/add-parameter "param" "Some Description")
                       (sut/add-switch-option "a" "a-option" "Option A"))]
          (should= "[options] <param>" (sut/arg-string spec))))

      (it "two parameters and switch"
        (let [spec (-> (constructor)
                       (sut/add-parameter "param" "Some Description")
                       (sut/add-switch-option "a" "a-option" "Option A")
                       (sut/add-parameter "another-param" "Some Description"))]
          (should= "[options] <param> <another-param>" (sut/arg-string spec))))

      (it "two parameters, switch, and optional"
        (let [spec (-> (constructor)
                       (sut/add-parameter "param" "Some Description")
                       (sut/add-switch-option "a" "a-option" "Option A")
                       (sut/add-parameter "another-param" "Some Description")
                       (sut/add-optional-parameter "param3" "Parameter 3"))]
          (should= "[options] <param> <another-param> [param3]" (sut/arg-string spec))))

      (it "two parameters, switch, optional, and multi parameter"
        (let [spec (-> (constructor)
                       (sut/add-parameter "param" "Some Description")
                       (sut/add-switch-option "a" "a-option" "Option A")
                       (sut/add-parameter "another-param" "Some Description")
                       (sut/add-optional-parameter "param3" "Parameter 3")
                       (sut/add-multi-parameter "param4" "Parameter 4"))]
          (should= "[options] <param> <another-param> [param3] [param4*]" (sut/arg-string spec))))

      (it "with optional parameters"
        (let [spec (-> (constructor)
                       (sut/add-optional-parameter "param" "Some Description"))]
          (should= "[param]" (sut/arg-string spec))))
      )

    (context "parameters-string"

      (it "empty"
        (should= "" (sut/parameters-string (constructor))))

      (it "one parameter"
        (parameter-string-should=
          (-> (constructor)
              (sut/add-parameter "foo" "Foo Param"))
          "  foo  Foo Param"))

      (it "two parameters"
        (parameter-string-should=
          (-> (constructor)
              (sut/add-parameter "foo" "Foo Param")
              (sut/add-parameter "fizz" "Fizz Param"))
          "  foo   Foo Param"
          "  fizz  Fizz Param"))
      )

    (context "option-string"

      (it "empty"
        (should= "" (sut/options-string (constructor))))

      (it "one switch option"
        (option-string-should=
          (-> (constructor)
              (sut/add-switch-option "a" "a-option" "Option A"))
          "  -a, --a-option  Option A"))

      (it "switch and value options"
        (option-string-should=
          (-> (constructor)
              (sut/add-switch-option "a" "a-option" "Option A")
              (sut/add-value-option "b" "b-option" "value" "Option B"))
          "  -a, --a-option          Option A"
          "  -b, --b-option=<value>  Option B"))

      (it "switch, value, and multi options"
        (option-string-should=
          (-> (constructor)
              (sut/add-switch-option "a" "a-option" "Option A")
              (sut/add-value-option "b" "b-option" "value" "Option B")
              (sut/add-multi-option "c" "c-option" "value" "Option C"))
          "  -a, --a-option          Option A"
          "  -b, --b-option=<value>  Option B"
          "  -c, --c-option=<value>  Option C"))

      (it "multiline options are aligned properly"
        (option-string-should=
          (-> (constructor)
              (sut/add-switch-option "a" "a-option" "Option A")
              (sut/add-value-option "b" "b-option" "value" "Option B\nmore info on b option"))
          "  -a, --a-option          Option A"
          "  -b, --b-option=<value>  Option B"
          "                          more info on b option"))

      (it "long option descriptions are split into multiple lines"
        (option-string-should=
          (-> (constructor)
              (sut/add-switch-option "a" "a-option" "Option A")
              (sut/add-value-option "b" "b-option" "value" "Option B which has a really long description that should be cutoff at 72 chars."))
          "  -a, --a-option          Option A"
          "  -b, --b-option=<value>  Option B which has a really long description that should be cutoff at 72"
          "                          chars."))

      (it "extra newlines are preserved in options string"
        (option-string-should=
          (-> (constructor)
              (sut/add-switch-option "a" "a-option" "Option A")
              (sut/add-value-option "b" "b-option" "value" "Option B\n\nThat's it"))
          "  -a, --a-option          Option A"
          "  -b, --b-option=<value>  Option B"
          "                          "
          "                          That's it"))
      )
    )
  )