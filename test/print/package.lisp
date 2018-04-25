;;;; package.lisp --- Package definition for unit tests of the text.source-location.print system.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:text.source-location.print.test
  (:use
   #:cl

   #:fiveam

   #:text.source-location
   #:text.source-location.print)

  (:export
   #:run-tests)

  (:documentation
   "Unit tests for the text.source-location.print system."))

(cl:in-package #:text.source-location.print.test)

(def-suite :text.source-location.print)

(defun run-tests ()
  (run! :text.source-location.print))
