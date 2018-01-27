;;;; positions.lisp --- Functions for handling positions in text
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.source-location)

;;; Utilities

(declaim (inline make-newline-vector))
(defun make-newline-vector (&optional (length 10))
  (make-array length :element-type 'array-index :adjustable t :fill-pointer 0))

(declaim (ftype (function ((and string (not (vector nil))) &optional vector) vector)
                newlines))
(defun newlines (text &optional newlines)
  "Return a vector containing positions of newlines in TEXT.

   If NEWLINES is supplied, it has to be an adjustable vector with a
   fill pointer."
  (declare (optimize speed (debug 1) (safety 1)))
  (if (zerop (length text))
      (make-newline-vector 0)
      (loop :with newlines = (or newlines
                                 (make-newline-vector))
         :for previous = 0 :then next
         :for next = (position #\Newline text :start (1+ previous))
         :while next
         :do (vector-push-extend next newlines (max 10 (length newlines)))
         :finally (return newlines))))

(declaim (inline make-text-info))
(defstruct (text-info
             (:constructor make-text-info (newlines length)))
  (newlines nil :type (array array-index 1) :read-only t)
  (length   nil :type array-index           :read-only t))

(defun text-info (text)
  (make-text-info (newlines text) (length text)))

;;; Line and Column <-> Index

(defmethod line+column ((thing integer) (text string))
  (line+column thing (text-info text)))

(defmethod line+column ((thing integer) (text text-info))
  (let* ((newlines      (text-info-newlines text))
         (line          (position thing newlines :test #'> :from-end t)) ; TODO binary search
         (newline-index (if line
                            (aref newlines line)
                            -1)))
    (values (if line (+ line 1) 0)
            (- thing newline-index 1))))

(defmethod line+column ((thing index-position) (text t))
  (line+column (index thing) text))

(defmethod line+column ((thing line+column-position) (text t))
  (values (line thing) (column thing)))

(defmethod line+column->index ((line   integer)
                               (column integer)
                               (text   string))
  (line+column->index line column (text-info text)))

(defmethod line+column->index ((line   integer)
                               (column integer)
                               (text   text-info))
  (+ (if (plusp line)
         (1+ (aref (text-info-newlines text) (1- line)))
         0)
     column))

;;;

(defmethod line-bounds ((thing t) (text string))
  (line-bounds thing (text-info text)))

(defmethod line-bounds ((thing integer) (text text-info))
  (let* ((newlines   (text-info-newlines text))
         (line-count (length newlines))
         (length     (text-info-length text)))
    (values (cond
              ((zerop thing)
               0)
              ((> thing line-count)
               length)
              (t
               (1+ (aref newlines (1- thing)))))
            (cond
              ((>= thing line-count)
               length)
              (t
               (aref newlines thing))))))

(defmethod line-bounds ((thing range) (text text-info))
  (let+ (((&values start-line start-column) (line+column (start thing) text))
         ((&values end-line   end-column)   (line+column (end   thing) text)))
    (values              (line-bounds start-line text)
            (nth-value 1 (line-bounds end-line   text))
            start-line start-column
            end-line   end-column)))

;;; Lines

(defmethod lines ((thing range) (text string))
  (let+ ((info (text-info text))
         ((&values start end start-line start-column end-line end-column)
          (line-bounds thing info))
         (start-in-first-line (- start (line-bounds start-line info)))
         (end-in-last-line    (- (nth-value 1 (line-bounds end-line info)) end)))
    (values (loop :for line :from start-line :to end-line
                  :collect (multiple-value-call #'subseq
                             text (line-bounds line info)))
            start-line   end-line
            start-column end-column
            start        end)))
