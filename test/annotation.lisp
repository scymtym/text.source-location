;;;; annotation.lisp --- Unit tests for annotation class.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.source-location.test)

(def-suite* :text.source-location.annotation
  :in :text.source-location)

(test annotation.print
  "Test printing `annotation' instances."

  (mapc (lambda+ ((args expected))
          (let ((annotation (apply #'make-annotation args)))
            (is (string= expected (with-output-to-string (stream)
                                    (print-items:format-print-items
                                     stream (print-items:print-items
                                             annotation)))))))

        `(((,(make-location "foo" 1 2) "foo") "<string>: 1-2 NOTE foo"))))

(test make-annotation.smoke
  "Smoke test for the `make-annotation' function."

  (mapc (lambda+ ((args expected-text expected-kind))
          (let ((annotation (apply #'make-annotation args)))
            (is (equal expected-text (text annotation)))
            (is (eq    expected-kind (kind annotation)))))

        `(((,(make-location "foo" 1 2) "here") "here" :note))))
