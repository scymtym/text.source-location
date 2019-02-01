;;;; util.lisp --- Utilities used in the print module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.source-location.print)

(defun safe-pathname (pathname)
  ;; Pathname with `:back' components have no namestring (at least in
  ;; SBCL). We therefore replace `:back' with `:up' when presenting
  ;; pathnames.
  (let ((safe-directory (substitute :up :back (pathname-directory pathname))))
    (make-pathname :directory safe-directory
                   :defaults  pathname)))

(defun safe-source-name (source)
  (let ((name (name source)))
    (typecase name
      (pathname (safe-pathname name))
      (t        name))))
