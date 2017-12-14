(defsystem "text.source-location.source-tracking-builder"
  :description "TODO"
  :license     "LLGPLv3" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "let-plus"                      "0.2")
                (:version "more-conditions"               "0.1")

                (:version "architecture.builder-protocol" "0.7")

                (:version "text.source-location"          (:read-file-form "version-string.sexp")))

  :components  ((:module     "src"
                 :components ((:file       "source-tracking-builder"))))
  ; :in-order-to ((test-op (test-op "text.source-location.source-tracking-builder/test")))
  )

#+later (defsystem "text.source-location.source-tracking-builder/test"
  :depends-on ((:version "fiveam"               "1.3")

               (:version "text.source-location" (:read-file-form "version-string.sexp")))
  :perform    )
