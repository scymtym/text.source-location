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
                (:static-file "README.org")))
