;;;; locations.lisp --- Representation and utilities for source locations.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.source-location)

;;; `info-mixin'

(defclass info-mixin ()
  ((%info ; :type    (or text-info function)
    :accessor %info))
  (:default-initargs
   :info (missing-required-initarg 'info-mixin :info)))

(defmethod shared-initialize :after ((instance   info-mixin)
                                     (slot-names t)
                                     &key
                                     (info nil info-supplied?))
  (when info-supplied?
    (setf (%info instance)
          (etypecase info
            (string   (curry #'text-info info))
            (function info)
            (t ;; text-info
             info)))))

(defmethod info ((position info-mixin))
  (let ((info (%info position)))
    (if (functionp info)
        (setf (%info position) (funcall info))
        info)))

(defmethod attach-text ((position info-mixin) (text t))
  position)

;;; `index-position'

(defclass index-position (print-items:print-items-mixin)
  ((index :initarg :index
          :type    non-negative-integer
          :reader  index))
  (:default-initargs
   :index (missing-required-initarg 'index-position :index)))

(defmethod print-items:print-items append ((object index-position))
  `((:index ,(index object) "~D")))

(defmethod attach-text ((position index-position) (text t))
  (change-class position 'index+info-position :info text))

;;; `index+info-position'

(defclass index+info-position (index-position info-mixin)
  ())

(defmethod line ((position index+info-position))
  (nth-value 0 (line+column position (info position))))

(defmethod column ((position index+info-position))
  (nth-value 1 (line+column position (info position))))

;;; `line+column-position'

(defclass line+column-position (print-items:print-items-mixin)
  ((line   :initarg  :line
           :type     non-negative-integer
           :reader   line)
   (column :initarg  :column
           :type     non-negative-integer
           :reader   column))
  (:default-initargs
   :line   (missing-required-initarg 'line+column-position :line)
   :column (missing-required-initarg 'line+column-position :column)))

(defmethod index ((position line+column-position))
  (error "not available"))

(defmethod print-items:print-items append ((object line+column-position))
  `((:line      ,(line object)   "~D")
    (:separator nil              ":"  ((:after :line)))
    (:column    ,(column object) "~D" ((:after :separator)))))

(defmethod location< ((left  line+column-position)
                      (right line+column-position))
  (let ((line-left  (line left))
        (line-right (line right)))
    (or (< line-left line-right)
        (and (= line-left line-right)
             (< (column left) (column right))))))

(defmethod location= ((left  line+column-position)
                      (right line+column-position)
                      &key)
  (and (= (line left) (line right))
       (= (column left) (column right))))

(defmethod attach-text ((position line+column-position) (text t))
  (change-class position 'line+column+info-position :info text))

;;; `line+column+info-position'

(defclass line+column+info-position (line+column-position
                                     info-mixin)
  ((%info ; :type    (or text-info function)
          :accessor %info))
  (:default-initargs
   :info (missing-required-initarg 'line+column+info-position :info)))

(defmethod shared-initialize :after ((instance   line+column+info-position)
                                     (slot-names t)
                                     &key
                                     (info nil info-supplied?))
  (when info-supplied?
    (setf (%info instance)
          (etypecase info
            (string   (curry #'text-info info))
            (function info)
            (t ;; text-info
             info)))))

(defmethod info ((position line+column+info-position))
  (let ((info (%info position)))
    (if (functionp info)
        (setf (%info position) (funcall info))
        info)))

(defmethod index ((position line+column+info-position))
  (line+column->index (line position) (column position) (info position)))

;;; `location' class

(defclass location (print-items:print-items-mixin)
  ((source :initarg  :source
           :accessor source             ; TODO read-only?
           :initform nil
           :documentation
           "Stores the source that was being parsed when the error
            occurred.")
   (range  :initarg  :range
           :type     (or null range)
           :accessor range              ; TODO read-only?
           :initform nil                ; TODO required?
           :documentation
           "Optionally stores bounds of interesting region within
            source string."))
  (:documentation
   "A location within a source string."))

(defmethod shared-initialize :after ((instance   location)
                                     (slot-names t)
                                     &key
                                     bounds
                                     position)
  #+no (cond
    ((and bounds position)
     (incompatible-initargs 'location
                            :bounds   bounds
                            :position position))

    (position
     (check-type position non-negative-integer)
     (setf (range instance) (make-range position))))

  #+no (let+ (((&accessors-r/o source-content bounds) instance)
         ((&flet check-position (position &key (inclusive? t))
                 (unless (or (not position) (not source-content)
                             (<= 0 position (max 0 (- (length source-content)
                                                      (if inclusive? 1 0)))))
                   (incompatible-initargs 'location
                                          :source-content source-content
                                          :range          bounds)))))
        (when bounds
          (check-type bounds bounds/cons)
          (check-position (car bounds) :inclusive? t)
          (check-position (cdr bounds) :inclusive? nil))))

(defmethod start ((range location))
  (start (range range)))

(defmethod end ((range location))
  (end (range range)))

(defmethod bounds ((range location))
  (bounds (range range)))

(defmethod content ((source location))
  (subseq (content (source source))
          (index (start source)) ; TODO can we use `bounds' here?
          (index (end source))))

(defmethod print-items:print-items append ((object location))
  (list* (find :source-name (print-items:print-items (source object))
               :key #'first)
         '(:location-separator nil ": " ((:after  :source-name)
                                         (:before :start)))
         (print-items:print-items (range object))))

(defun make-location (source start end &key source-content)
  (let ((source (if (typep source 'source) ; TODO hack
                    source
                    (make-source source :content source-content))))
    (make-instance 'location
                   :source source
                   :range  (make-range start end (content source)))))

(defmethod location< ((left location) (right location))
  (location< (start left) (start right)))

(defmethod location= ((left  location)
                      (right location)
                      &key
                      (compare-source?         t)
                      (compare-source-content? t)
                      (compare-bounds?         t))
  (and (or (not compare-source?)
           (equal (source left) (source right))) ; TODO source=
       (or (not compare-bounds?)
           (equal (range left) (range right)))))
