(defsystem "text.source-location"
  :description "Facilities for managing and presenting locations in text"
  :license     "LLGPLv3" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ()

  :components  ((:module     "src"
                 :components ((:file       "package")))

                (:static-file "COPYING")
                (:static-file "README.org"))

  :in-order-to ((test-op (test-op "text.source-location/test"))))

(defsystem "text.source-location/test"
  :depends-on ((:version "fiveam"               "1.4")

               (:version "text.source-location" (:read-file-form "version-string.sexp")))
  :components ((:module      "test"
                :components ((:file       "package"))))
  :perform    (test-op (operation component)
                (symbol-call '#:text.source-location.test '#:run-tests)))
