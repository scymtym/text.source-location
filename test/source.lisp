;;;; source.lisp --- Unit tests for the source class.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.source-location.test)

(def-suite* :text.source-location.source
  :in :text.source-location)

(test source.print
  "Test printing `source' instances."

  (mapc (lambda+ ((args expected))
          (let ((source (apply #'make-source args)))
            (is (string= expected
                         (with-output-to-string (stream)
                           (print-items:format-print-items
                            stream (print-items:print-items source)))))))

        `((("foo")                                   "<string> \"foo\"")
          (("foo"             :content "bar")        "foo \"bar\"")
          ((,(make-string 100 :initial-element #\a)) "<string> \"aaaaaaaaaaaaaaaaaaaa…\"")

          ((,*standard-input*)                       "<stream>")
          ((,*standard-input* :content "bar")        "<stream> \"bar\"")

          ((,#P"foo.baz")                            "foo.baz")
          ((,#P"foo.baz"      :content "bar")        "foo.baz \"bar\""))))

(test make-source.smoke
  "Test making `source' instances using `make-source'."

  (mapc (lambda+ ((args expected-name expected-content))
          (let ((source (apply #'make-source args)))
            (is (equal expected-name    (name source)))
            (is (equal expected-content (content source)))))
        `((("foo")                            "<string>"   "foo")
          (("foo" :content "bar")             "foo"        "bar")

          ((,*standard-input*)                "<stream>"   nil)
          ((,*standard-input* :content "bar") "<stream>"   "bar")

          ((,#P"foo.txt")                     ,#P"foo.txt" nil)
          ((,#P"foo.txt" :content "bar")      ,#P"foo.txt" "bar"))))
