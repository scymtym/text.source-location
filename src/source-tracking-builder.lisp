(cl:defpackage #:text.source-location.source-tracking-builder
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:import-from #:more-conditions
   #:missing-required-initarg)

  (:local-nicknames
   (#:bp #:architecture.builder-protocol))

  ;; Source tracking builder protocol
  (:export
   #:input
   #:target)

  ;; Source tracking builder creation protocol
  (:export
   #:source-tracking-builder
   #:make-source-tracking-builder))

(cl:in-package  #:text.source-location.source-tracking-builder)

;;; Helper structure

(defstruct (partial-node
             (:constructor make-partial-node (node source)))
  (node   nil)
  (source nil :read-only t))

;;; The builder

(defclass source-tracking-builder ()
  ((input  :initarg  :input
           :accessor input)
   (target :initarg  :target
           :reader   target))
  (:default-initargs
   :input  (missing-required-initarg 'source-tracking-builder :input)
   :target (missing-required-initarg 'source-tracking-builder :target)))

(declaim (inline make-source-tracking-builder))
(defun make-source-tracking-builder (input target)
  (make-instance 'source-tracking-builder :input input :target target))

(defmethod bp:make-node ((builder source-tracking-builder)
                         (kind    t)
                         &rest initargs &key bounds)
  (let+ ((node (apply #'bp:make-node (target builder) kind
                      (remove-from-plist initargs :bounds)))
         #+later ((start . end) bounds)
         #+later (location (make-location (input builder) start end)))
    (make-partial-node node bounds #+no location)))

(defmethod bp:relate ((builder  source-tracking-builder)
                      (relation t)
                      (left     partial-node)
                      (right    t)
                      &rest initargs &key)
  ; (log:info left right)
  (let+ (((&structure partial-node- node) left))
    (setf node (apply #'bp:relate (target builder) relation node right
                      initargs))
    left))

(defmethod bp:finish-node ((builder source-tracking-builder)
                           (kind    t)
                           (node    partial-node))
  ; (log:info kind node)
  (let+ (; ((&structure partial-node- node source) node)
         (node1 (bp:finish-node (target builder) kind (partial-node-node node))))
    (model.transform.trace:recording-transform (() (partial-node-source node)) node1)))
