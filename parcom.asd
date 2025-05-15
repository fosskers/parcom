(defsystem "parcom"
  :version "1.1.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :homepage "https://github.com/fosskers/parcom"
  :description "A simple parser combinator library."
  :depends-on ()
  :components ((:module "src"
                :components ((:file "package")
                             (:file "fp")
                             (:file "parsers")
                             (:file "combinators"))))
  :in-order-to ((test-op (test-op :parcom/tests))))

(defsystem "parcom/json"
  :depends-on (:parcom)
  :components ((:module "src" :components ((:file "json"))))
  :in-order-to ((test-op (test-op :parcom/tests))))

(defsystem "parcom/datetime"
  :depends-on (:parcom)
  :description "RFC 3339 dates and times."
  :components ((:module "src" :components ((:file "datetime"))))
  :in-order-to ((test-op (test-op :parcom/tests))))

(defsystem "parcom/toml"
  :depends-on (:parcom :parcom/datetime)
  :components ((:module "src" :components ((:file "toml"))))
  :in-order-to ((test-op (test-op :parcom/tests))))

(defsystem "parcom/tests"
  :depends-on (:parcom :parcom/json :parcom/datetime :parcom/toml :parachute)
  :components ((:module "tests" :components ((:file "tests"))))
  :perform (test-op (op c) (symbol-call :parachute :test :parcom/tests)))

(defsystem "parcom/benchmarks"
  :depends-on (:parcom :parcom/json :com.inuoe.jzon :shasht :jsown :yason)
  :components ((:module "tests" :components ((:file "bench")))))
