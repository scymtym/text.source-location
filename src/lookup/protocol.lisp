;;;; protocol.lisp --- Protocol provided by the lookup module.
;;;;
;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.source-location.lookup)

;; TODO could be define in text.source-location system?

(defgeneric location-in? (location range)
  (:documentation
   "Return true if LOCATION is contained in RANGE."))

;;; Default behavior

(defmethod location-in? ((location t) (range sloc:range)) ; TODO add to sloc package?
  (let+ (((&values start end) (sloc:bounds range)))
    (and (or (sloc:location< start location) ; TODO location<=
             (sloc:location= start location))
         (or (sloc:location< location end) ; TODO this should be < in some cases and <= in others. keyword parameter?
             (sloc:location= location end)))))

(defmethod location-in? ((location sloc:range) (range sloc:range))
  (let+ (((&values start end) (sloc:bounds location)))
    (and (location-in? start range) (location-in? end range))))

(defmethod location-in? ((location sloc:location) (range t))
  (location-in? (sloc:range location) range))

(defmethod location-in? ((location t) (range sloc:location))
  (location-in? location (sloc:range range)))

(defmethod location-in? ((location sloc:location) (range sloc:location))
  (and (eq (sloc:source location) (sloc:source range))
       (call-next-method)))

;;; Lookup protocol

;; TODO rename to `at'?
(defgeneric lookup (location index &key if-multiple)
  (:documentation
   "Return locations in INDEX that contain LOCATION.

    In cases multiple locations in INDEX contain LOCATION, IF-MULTIPLE
    is called on the sequence containing these locations to compute
    the return value."))

(defgeneric in (range index &key if-multiple)
  (:documentation
   "Return locations in INDEX contained in RANGE.

    In cases multiple locations in INDEX are contained in RANGE,
    IF-MULTIPLE is called on the sequence containing these locations
    to compute the return value."))

;;; Index mutation protocol

(defgeneric add! (location index)
  (:documentation
   "Add LOCATION to INDEX. Return updated INDEX."))

(defgeneric remove! (location index)
  (:documentation
   "Remove LOCATION from INDEX. Return updated INDEX."))
