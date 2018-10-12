(asdf:defsystem "text.source-location.source-tracking-builder"
  :description "A builder that attaches source locations to nodes"
  :license     "LLGPLv3" ; see COPYING for details

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                (:version "let-plus"                      "0.2")
                (:version "more-conditions"               "0.1")

                (:version "architecture.builder-protocol" "0.7")

                (:version "text.source-location"          (:read-file-form "version-string.sexp"))

                "model.transform.trace")

  :components  ((:module     "src"
                 :components ((:file       "source-tracking-builder"))))

  :in-order-to ((test-op (test-op "text.source-location.source-tracking-builder/test"))))

(asdf:defsystem "text.source-location.source-tracking-builder/test"

  :version    (:read-file-form "version-string.sexp")
  :depends-on ((:version "fiveam"                                       "1.4")

               (:version "text.source-location"                         (:read-file-form "version-string.sexp"))
               (:version "text.source-location.source-tracking-builder" (:read-file-form "version-string.sexp")))

  :components ((:module     "test"
                :components ((:file        "source-tracking-builder"))))

  :perform    (test-op (operation component)
                (uiop:symbol-call '#:text.source-location.source-tracking-builder.test
                                  '#:run-tests)))
