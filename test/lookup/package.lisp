;;;; package.lisp --- Package definition for unit tests of the text.source-location.lookup system.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:text.source-location.lookup.test
  (:use
   #:cl

   #:fiveam

   #:text.source-location
   #:text.source-location.lookup)

  (:export
   #:run-tests)

  (:documentation
   "Unit tests for the text.source-location.lookup system."))

(cl:in-package #:text.source-location.lookup.test)

(def-suite :text.source-location.lookup)

(defun run-tests ()
  (run! :text.source-location.lookup))
