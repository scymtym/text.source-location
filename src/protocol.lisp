;;;; protocol.lisp --- Protocol functions provided by the text.source-location system.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.source-location)

;;; Source protocol

(defgeneric name (source)
  (:documentation
   "Return the name of SOURCE."))

(defgeneric content (source)
  (:documentation
   "Return the content of SOURCE or NIL if not available."))

;;; Location comparison protocol

(defgeneric location< (left right)
  (:documentation
   "TODO"))

(defgeneric location= (left right &key &allow-other-keys) ; TODO think about &key
  (:documentation
   "TODO"))

;;; Default behavior

(defmethod location< ((left t) (right t))
  (< (index left) (index right)))

(defmethod location= ((left t) (right t) &key)
  (= (index left) (index right)))

;;; Position protocol

(defgeneric index (position))

;;; Range protocol

(defgeneric start (range)
  (:documentation
   "Return the start position of RANGE."))

(defgeneric end (range)
  (:documentation
   "Return the end position of RANGE."))

(defgeneric bounds (range)
  (:documentation
   "Return the start and end positions of RANGE as two values."))

(defgeneric size (range)) ; TODO good name?

;;; Default behavior

(defmethod size ((range t))
  (- (index (end range)) (index (start range))))

;;; Location protocol

(defgeneric source (location)
  (:documentation
   "Return the source of LOCATION.

    The returned object can be used with the source protocol."))

(defgeneric range (location)
  (:documentation
   "Return the range of LOCATION.

    The returned object can be used with the range protocol."))

;;; There should be a `content' method on `location' implementations

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
