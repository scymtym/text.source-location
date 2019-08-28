;;;; source.lisp --- Sources of text.
;;;;
;;;; Copyright (C) 2017, 2018, 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.source-location)

(defclass source (print-items:print-items-mixin)
  ((%name    :initarg :name
             :reader  name)
   (%content :initarg :content ; TODO should this be optional?
             :reader  content))
  (:default-initargs
   :name    (missing-required-initarg 'source :name)
   :content (missing-required-initarg 'source :content))
  (:documentation
   "A name optionally alongside the content of source."))

(defmethod print-items:print-items append ((object source))
  (let+ ((content           (content object))
         ((&values printable-content shortened?)
          (typecase content
            (string (printable-content content)))))
    `((:source-name ,(name object) "~A")
      ,@(when printable-content
          `((:open           nil                " \""      ((:after :source-name)))
            (:source-content ,printable-content "~A"       ((:after :open)))
            (:shortened?     ,shortened?        "~:[~;…~]" ((:after :source-content)))
            (:close          nil                "\""       ((:after :shortened?))))))))

(defun make-source (source &key content)
  (let+ (((&flet make-it (name content)
            (make-instance 'source :name name :content content)))
         ((&flet stream-name (stream)
            (or (ignore-errors (pathname stream)) "<stream>"))))
    (etypecase source
      (string   (if content
                    (make-it source     content)
                    (make-it "<string>" source)))
      (stream   (make-it (stream-name source) content))
      (pathname (make-it source               content)))))
