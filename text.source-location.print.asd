(defsystem "text.source-location.print"
  :description "Facilities for presenting locations in text"
  :license     "LLGPLv3" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "let-plus"                      "0.2")
                (:version "more-conditions"               "0.1")
                (:version "utilities.print-items"         "0.1")

                (:version "architecture.service-provider" "0.5")

                (:version "text.source-location"          (:read-file-form "version-string.sexp")))

  :components  ((:module     "print"
                 :pathname   "src/print"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")

                              (:file       "print")

                              (:file       "style-ansi-escapes"))))

  :in-order-to ((test-op (test-op "text.source-location.print/test"))))

(defsystem "text.source-location.print/test"
  :depends-on ((:version "fiveam"                     "1.4")

               (:version "text.source-location.print" (:read-file-form "version-string.sexp")))

  :components ((:module      "print"
                :pathname    "test/print"
                :components ((:file       "package"))))

  :perform    (test-op (operation component)
                (symbol-call '#:text.source-location.print.test '#:run-tests)))
