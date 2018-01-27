;;;; protocol.lisp --- Protocol function provided by the text.source-location system.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.source-location)

;;;

(defgeneric location< (left right)
  (:documentation
   "TODO"))

(defgeneric location= (left right &key &allow-other-keys) ; TODO think about &key
  (:documentation
   "TODO"))

;;; Position protocol

;;; Range protocol

(defgeneric start (range)
  (:documentation
   "TODO"))

(defgeneric end (range)
  (:documentation
   "TODO"))

;;; Location protocol

(defgeneric source (location))

(defgeneric source-content (location))

(defgeneric range (location))

;;; Annotation protocol

(defgeneric location (annotation))

(defgeneric text (annotation)) ; TODO could be any object

;;; Line and column protocol

(defgeneric line+column (thing text))

(defgeneric line+column->index (line column text))

(defgeneric line-bounds (thing text)
  (:argument-precedence-order text thing))

(defgeneric lines (thing text))
