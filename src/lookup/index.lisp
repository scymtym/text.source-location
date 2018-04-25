(cl:in-package #:text.source-location.lookup)

(defclass index (print-items:print-items-mixin)
  ((locations :accessor %locations
              :initform '())))

(defun make-index ()
  (make-instance 'index))

(defmethod print-items:print-items append ((object index))
  `((:location-count ,(length (%locations object)) "~:D location~:P")))

(let+ (((&flet %lookup (location index test if-multiple)
          (declare (type function test))
          (let ((result (reduce (lambda+ (result (range . value))
                                  (if (funcall test location range)
                                      (list* value result)
                                      result))
                                (%locations index) :initial-value '())))
            (if (length= 1 result)
                result
                (funcall if-multiple result)))))
       (ascending  (rcurry #'sort #'< :key #'sloc:size))
       (descending (rcurry #'sort #'> :key #'sloc:size)))

  (defmethod lookup ((location t) (index index) &key (if-multiple ascending))
    (%lookup location index #'location-in? if-multiple))

  (defmethod in ((range t) (index index) &key (if-multiple descending))
    (%lookup range index
             (lambda (location range) (location-in? range location))
             if-multiple)))

(defmethod add! ((location sloc:location) (index index))
  (push (cons (sloc:range location) location) (%locations index))
  index)

(defmethod remove! ((location sloc:location) (index index))
  ;; TODO implement
  (error "not implemented")
  index)
