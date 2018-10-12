;;;; package.lisp --- Package definition for the lookup module.
;;;;
;;;; Copyright (C) 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:text.source-location.lookup
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:local-nicknames
   (#:sloc #:text.source-location))

  ;;
  (:export
   #:location-in?)                   ; TODO add to text.s-loc package?

  ;; Lookup protocol
  (:export
   #:lookup
   #:in)

  ;; Index mutation protocol
  (:export
   #:add!
   #:remove!)

  ;; Index creation
  (:export
   #:make-range-index
   #:make-location-index

   #:make-index)

  (:documentation
   "Index data structures for locations and ranges."))
