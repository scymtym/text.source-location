;;;; range.lisp --- Runs of characters in a source.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.source-location)

(defclass range (print-items:print-items-mixin)
  ((start :initarg :start
          ;;:type    position
          :reader  start)
   (end   :initarg :end
          ;;:type    position
          :reader  end))
  (:default-initargs
   :start (missing-required-initarg 'range :start)
   :end   (missing-required-initarg 'range :end)))

(defmethod bounds ((range range))
  (values (start range) (end range)))

;; TODO assert (location< start end)

(declaim (inline make-range))
(defun make-range (start &optional (end start) info)
  (let+ (((&flet make-bound (bound)
            (typecase bound
              (integer (if info
                           (make-instance 'index+info-position
                                          :index bound
                                          :info  info)
                           (make-instance 'index-position :index bound)))
              (t       bound)))))
    (make-instance 'range
                   :start (make-bound start)
                   :end   (make-bound end))))

(defmethod print-items:print-items append ((object range))
  (let ((start (print-items:print-items (start object)))
        (end   (print-items:print-items (end object))))
    `((:start           ,start "~/print-items:format-print-items/")
      (:range-separator nil    "-"
                        ((:after :start)))
      (:end             ,end   "~/print-items:format-print-items/"
                        ((:after :range-separator))))))

(defmethod location< ((left range) (right range))
  (location< (start left) (end right)))

(defmethod attach-text ((position range) (text t))
  (attach-text (start position) text)
  (attach-text (end   position) text)
  position)
