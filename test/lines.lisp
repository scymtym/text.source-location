;;;; lines.lisp --- Unit tests for line and column-related functions.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.source-location.test)

(in-suite :text.source-location)

(test line+column.smoke

  (loop :for i :to 6 :collect
       (multiple-value-list (line+column i "foo
bar")))

  (let ((source (format nil "foo bar baz fez who dip bar bar di  do ")))
    (loop :for i :from 0 :to 17
       :collect (multiple-value-list (line+column i source))))


  (let ((source (format nil "foo bar~@
                             baz fez~@
                             who dip~@
                             bar bar~@
                             di  do ")))
    (loop :for i :from 0 :to 17
       :collect (multiple-value-list (line+column i source)))))

(test line-bounds.smoke

  (line-bounds (make-range 1 5)
               "foo
bar"))

(test lines.smoke

  (lines (make-range (make-instance 'text.source-location::index-position :index 1)
                     (make-instance 'text.source-location::index-position :index 5))
         "foo
bar"))
