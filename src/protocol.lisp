;;;; protocol.lisp --- Protocol function provided by the text.source-location system.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.source-location)

;;; Source protocol

(defgeneric name (source)
  (:documentation
   "TODO"))

(defgeneric content (source)
  (:documentation
   "TODO"))

;;; Location comparison protocol

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

(defgeneric bounds (range)
  (:documentation
   "TODO"))

;;; Location protocol

(defgeneric source (location))

(defgeneric range (location))

;;; Annotation protocol

(defgeneric location (annotation)
  (:documentation
   "TODO"))

(defgeneric text (annotation)  ; TODO could be any object, not just text
  (:documentation
   "TODO"))

;;; Line and column protocol

(defgeneric line+column (thing text))

(defgeneric line+column->index (line column text))

(defgeneric line-bounds (thing text)
  (:argument-precedence-order text thing))

(defgeneric lines (thing text))
