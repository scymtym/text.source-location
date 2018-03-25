(defsystem "text.source-location.lookup"
  :description "Infrastructure for finding nodes associated to source locations"
  :license     "LLGPLv3" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "let-plus"             "0.2")

                (:version "text.source-location" (:read-file-form "version-string.sexp")))

  :components  ((:module     "lookup"
                 :pathname   "src/lookup"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "index"))))

  :in-order-to ((test-op (test-op "text.source-location.lookup/test"))))

(defsystem "text.source-location.lookup/test"
  :depends-on ((:version "fiveam"                      "1.4")

               (:version "text.source-location.lookup" (:read-file-form "version-string.sexp")))

  :components ((:module      "test"
                :components ((:file       "package")
                             (:file       "protocol")
                             (:file       "index"))))
  :perform    (test-op (operation component)
                (symbol-call '#:text.source-location.lookup.test '#:run-tests)))
