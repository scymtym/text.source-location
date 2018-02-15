(cl:in-package #:text.source-location.test)

(def-suite* :text.source-location.annotation
  :in :text.source-location)

(test annotation.print
  "Test printing `annotation' instances."

  (mapc (lambda+ ((args expected))
          (is (string= expected (with-output-to-string (stream)
                                  (print-items:format-print-items
                                   stream (print-items:print-items
                                           (apply #'make-annotation args)))))))
        `(((,(make-location "foo" 1 2) "foo") "<string>: 1-2 NOTE foo"))))

(test make-annotation.smoke
  ""

  (make-annotation (make-location "foooooooooooooooooooooo" 10 15)
                   "declared here"))
