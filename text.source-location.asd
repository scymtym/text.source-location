(defsystem "text.source-location"
  :description "Facilities for managing locations in text"
  :license     "LLGPLv3" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "let-plus"              "0.2")
                (:version "more-conditions"       "0.1")
                (:version "utilities.print-items" "0.1"))

  :components  ((:module     "src"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "protocol")

                              (:file       "source")
                              (:file       "locations")
                              (:file       "annotation")

                              (:file       "lines")))

                (:static-file "COPYING")
                (:static-file "README.org"))

  :in-order-to ((test-op (test-op "text.source-location/test"))))

(defsystem "text.source-location/test"
  :depends-on ((:version "fiveam"               "1.4")

               (:version "text.source-location" (:read-file-form "version-string.sexp")))

  :components ((:module      "test"
                :serial      t
                :components ((:file       "package")

                             (:file       "source")
                             (:file       "annotation")

                             (:file       "lines")

                             (:file       "smoke"))))

  :perform    (test-op (operation component)
                (symbol-call '#:text.source-location.test '#:run-tests)))
