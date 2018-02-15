;;;; annotation.lisp --- .
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.source-location)

;;; `annotation'

(deftype annotation-kind ()
  `(member :info :note :warning :error))

(defclass annotation (print-items:print-items-mixin)
  ((location :initarg  :location
             :reader   location)
   (text     :initarg  :text
             :type     string
             :reader   text)
   (kind     :initarg  :kind
             :type     annotation-kind
             :reader   kind
             :initform :note))
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
    (:kind       ,(kind object) " ~A" ((:after :end)))
    (:annotation ,(text object) " ~A" ((:after :kind)))))

(defun make-annotation (location text)
  (make-instance 'annotation
                 :location location
                 :text     text))
