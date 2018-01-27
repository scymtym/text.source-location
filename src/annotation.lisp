;;;; annotation.lisp --- .
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.source-location)

;;;

(defclass annotation ()
  ((location :initarg :location
             :reader  location)
   (text     :initarg :text
             :type    string
             :reader  text))
  (:default-initargs
   :location (missing-required-initarg 'annotation :location)
   :text     (missing-required-initarg 'annotation :text)))

;;; annotated-locations

(defclass annotated-locations ()
  ())
