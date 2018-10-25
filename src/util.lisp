;;;; util.lisp --- Utilities used in the text.source-location system.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.source-location)

(defun printable-content (string &key (length-limit 20))
  (let+ ((length (length string))
         ((&values end shortened?) (if (<= length length-limit)
                                       (values length       nil)
                                       (values length-limit t))))
    (values (substitute-if #\. (complement #'graphic-char-p)
                           (subseq string 0 end))
            shortened?)))
