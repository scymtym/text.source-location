(cl:in-package #:text.source-location)

(defclass source ()
  ((name     :initarg :name
             :reader  name)
   (content  :initarg :content
             :reader  content))
  (:default-initargs
   :name    (missing-required-initarg 'source :name)
   :content (missing-required-initarg 'source :content)))
