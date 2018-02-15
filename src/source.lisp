;;;; source.lisp --- Sources of text.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.source-location)

(defclass source (print-items:print-items-mixin)
  ((name    :initarg :name
            :reader  name)
   (content :initarg :content ; TODO should this be optional?
            :reader  content))
  (:default-initargs
   :name    (missing-required-initarg 'source :name)
   :content (missing-required-initarg 'source :content)))

(defmethod print-items:print-items append ((object source))
  (let* ((content           (content object))
         (printable-content (typecase content
                              (string (substitute-if #\. (complement #'graphic-char-p)
                                                     (subseq content 0 (min 20 (length content))))))))
    `((:source-name ,(name object) "~A")
      ,@(when printable-content
          `((:source-content ,printable-content " \"~A\"" ((:after :source-name))))))))

(defun make-source (source &key content)
  (let+ (((&flet make-it (name content)
            (make-instance 'source :name name :content content)))
         ((&flet stream-name (stream)
            (or (ignore-errors (pathname stream)) "<stream>"))))
    (typecase source
      (string   (make-it "<string>"           (or content source)))
      (stream   (make-it (stream-name source) content))
      (pathname (make-it source               content)))))
