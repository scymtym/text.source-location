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

  ;; TODO where should this go? what should its name be?
  (:export
   #:text-info

   #:line+column
   #:line+column->index

   #:line-bounds
   #:lines)

  ;; Location comparison protocol
  (:export
   #:location<
   #:location=)

  ;; Experimental position protocol
  (:export
   #:index

   #:line
   #:column)

  ;; Source protocol
  (:export
   #:name
   #:content

   #:make-source)

  ;; Range protocol
  (:export
   #:start
   #:end
   #:bounds
   #:size

   #:make-range)

  ;; Location protocol
  (:export
   #:source
   #:range

   #:make-location)

  ;; Annotation protocol
  (:export
   #:location
   #:text
   #:kind

   #:make-annotation)

  ;; Utilities?
  (:export
   #:cluster-locations)

  (:documentation
   "Classes and functions for managing and presenting locations in text."))
