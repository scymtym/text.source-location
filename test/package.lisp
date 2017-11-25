;;;; package.lisp --- Package definition for unit tests of the text.source-location system.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:text.source-location.test
  (:use
   #:cl

   #:fiveam

   #:text.source-location)

  (:export
   #:run-tests)

  (:documentation
   "Unit test for the text.source-location system."))

(cl:in-package #:text.source-location.test)

(def-suite :text.source-location)

(defun run-tests ()
  (run! :text.source-location))
