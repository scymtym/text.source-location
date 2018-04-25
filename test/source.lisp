;;;; source.lisp --- Tests for the source class.
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
          (is (string= expected (with-output-to-string (stream)
                                  (print-items:format-print-items
                                   stream (print-items:print-items
                                           (apply #'make-source args)))))))
        '((("foo") "<string> \"foo\""))))

(test make-source.smoke
  "Test making `source' instances using `make-source'."

  (mapc (lambda+ ((args expected-name expected-content))
          (let ((source (apply #'make-source args)))
            (is (equal expected-name    (name source)))
            (is (equal expected-content (content source)))))
        `((("foo")                             "<string>"   "foo")
          ((,(make-string-input-stream "foo")) "<stream>"   nil)
          ((,#P"foo.txt")                      ,#P"foo.txt" nil))))
