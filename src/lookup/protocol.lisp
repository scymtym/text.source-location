(cl:in-package #:text.source-location.lookup)

;; TODO could be define in text.source-location system?

(defgeneric location-in? (location range)
  (:documentation
   "Return true if LOCATION is contained in RANGE."))

;;; Default behavior

(defmethod location-in? ((location t) (range sloc:range)) ; TODO add to sloc package?
  (let+ (((&values start end) (sloc:bounds range)))
    (and (or (sloc:location< start location)
             (sloc:location= start location))
         (sloc:location< location end))))

(defmethod location-in? ((location sloc:range) (range sloc:range))
  (let+ (((&values start end) (sloc:bounds location)))
    (and (location-in? start range) (location-in? end range))))

(defmethod location-in? ((location t) (range sloc:location))
  ;; TODO compare source if applicable?
  (location-in? location (sloc:range range)))

;;; Lookup protocol

(defgeneric lookup (location index &key if-overlap)
  (:documentation
   "Return a sequence of locations in INDEX that contain LOCATION."))

;;; Index mutation protocol

(defgeneric add! (location index)
  (:documentation
   "Add LOCATION to INDEX. Return updated INDEX."))

(defgeneric remove! (location index)
  (:documentation
   "Remove LOCATION from INDEX. Return updated INDEX."))
