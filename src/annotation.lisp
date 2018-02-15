;;;; annotation.lisp --- .
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.source-location)

;;; `annotation'

(defclass annotation (print-items:print-items-mixin)
  ((location :initarg :location
             :reader  location)
   (text     :initarg :text
             :type    string
             :reader  text))
  (:default-initargs
   :location (missing-required-initarg 'annotation :location)
   :text     (missing-required-initarg 'annotation :text))
  (:documentation
   "A location within a source together with an annotation text."))

(defmethod range ((location annotation))
  (range (location location)))

(defmethod start ((range annotation))
  (start (location range)))

(defmethod end ((range annotation))
  (end (location range)))

(defmethod bounds ((range annotation))
  (bounds (location range)))

(defmethod print-items:print-items append ((object annotation))
  `(,@(print-items:print-items (location object))
      (:annotation ,(text object) " ~A")))
