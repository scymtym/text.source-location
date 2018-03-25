(cl:in-package #:text.source-location.lookup)

(defclass index ()
  ((locations :accessor %locations
              :initform '())))

(defmethod lookup ((location t) (index index) &key if-overlap)
  (reduce (lambda+ (result (range . value))
            (if (location-in? location range)
                (list* value result)
                result))
          (%locations index) :initial-value '()))

(defmethod add! ((location sloc:location) (index index))
  (push (cons (sloc:range location) location) (%locations index))
  index)

(defmethod remove! ((location sloc:location) (index index))
  ;; TODO implement
  index)
