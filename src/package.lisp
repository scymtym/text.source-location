;;;; package.lisp --- Package definition for the text.source-location system.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:text.source-location
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:import-from #:more-conditions
   #:missing-required-initarg)

  ;; Source protocol
  (:export
   #:name
   #:content)

  (:export
   #:text-info

   #:line+column
   #:line+column->index

   #:line-bounds
   #:lines)

  ;; Experimental position protocol
  (:export
   #:index

   #:line
   #:column)

  ;; Range protocol
  (:export
   #:start
   #:end)

  ;; Location protocol
  (:export
   #:source
   #:source-content ; TODO first class source
   #:range)

  ;; Annotation protocol
  (:export
   #:location
   #:text)

  ;; Utilities?
  (:export
   #:cluster-locations)

  (:documentation
   "Classes and functions for managing and presenting locations in text."))
