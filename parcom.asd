(defsystem "parcom"
  :version "0.0.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :homepage "https://github.com/fosskers/parcom"
  :depends-on ()
  :components ((:module "src"
                :components ((:file "package")
                             (:file "fp")
                             (:file "combinators"))))
  :description "A simple parser combinator library.")
