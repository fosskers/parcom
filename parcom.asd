(defsystem "parcom"
  :version "0.0.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :homepage "https://github.com/fosskers/parcom"
  :depends-on ()
  :components ((:module "src"
                :components ((:file "package")
                             (:file "fp")
                             (:file "parsers")
                             (:file "combinators"))))
  :description "A simple parser combinator library."
  :in-order-to ((test-op (test-op :parcom/tests))))

(defsystem "parcom/tests"
  :depends-on (:parcom :parachute)
  :components ((:module "tests" :components ((:file "tests"))))
  :perform (test-op (op c) (symbol-call :parachute :test :parcom/tests)))
