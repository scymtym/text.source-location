(cl:in-package #:text.source-location.lookup)

;;; `range-index'

(defclass range-index (print-items:print-items-mixin)
  ((%ranges :accessor %ranges
            :initform '())))

(defmethod print-items:print-items append ((object range-index))
  `((:ranges-count ,(length (%ranges object)) "~:D range~:P")))

(defmethod %count ((index range-index))
  (length (%ranges index)))

(let+ (((&flet %lookup (range index test if-multiple)
          (declare (type function test))
          (let ((result (reduce (lambda+ (result (range* . value))
                                  (if (funcall test range range*)
                                      (list* value result)
                                      result))
                                (%ranges index) :initial-value '())))
                                        ; TODO if 0?
            (if (length= 1 result)
                result
                (funcall if-multiple result)))))
       (ascending  (rcurry #'sort #'< :key #'sloc:size))
       (descending (rcurry #'sort #'> :key #'sloc:size)))

  (defmethod lookup ((location t) (index range-index) &key (if-multiple ascending))
    (%lookup location index #'location-in? if-multiple))

  (defmethod in ((range t) (index range-index) &key (if-multiple descending))
    (%lookup range index
             (lambda (location range) (location-in? range location))
             if-multiple)))

(defmethod add! ((location t) (index range-index))
  (push (cons (sloc:range location) location) (%ranges index))
  index)

(defmethod remove! ((location sloc:location) (index range-index))
  ;; TODO implement
  (error "not implemented")
  index)

;;; `location-index'

(defclass location-index (print-items:print-items-mixin)
  ((%locations :accessor %locations
               :initform (make-hash-table :test #'eq))))

(defun make-index ()
  (make-instance 'location-index))

(defmethod %count ((index location-index))
  (let ((result 0))
    (maphash-values (lambda (value)
                      (incf result (%count value)))
                    (%locations index))
    result))

(defmethod print-items:print-items append ((object location-index))
  (let* ((locations (%locations object))
         (source-count   (hash-table-count locations))
         (location-count (%count object)))
    `((:location-count ,location-count "~:D location~:P")
      (:source-count   ,source-count   " in ~:D source~:P" ((:after :location-count))))))


(let ((ascending  (rcurry #'sort #'< :key #'sloc:size))
      (descending (rcurry #'sort #'> :key #'sloc:size)))

  (defmethod lookup ((location t) (index location-index) &key (if-multiple ascending))
    (let ((source (sloc:source location))
          (range  (sloc:range  location)))
      (when-let ((ranges (gethash source (%locations index))))
        (lookup range ranges :if-multiple if-multiple))))

  (defmethod in ((range t) (index location-index) &key (if-multiple descending))
    (let ((source (sloc:source range))
          (range  (sloc:range  range)))
      (when-let ((ranges (gethash source (%locations index))))
        (lookup range ranges :if-multiple if-multiple)))))

(defmethod add! ((location t) (index location-index))
  (let* ((source (sloc:source location))
         (ranges (ensure-gethash source (%locations index)
                                 (make-instance 'range-index))))
    (add! location ranges))
  index)

(defmethod remove! ((location sloc:location) (index location-index))
  ;; TODO implement
  (error "not implemented")
  index)
