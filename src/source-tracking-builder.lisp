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
   #:source
   #:target

   #:make-location)

  ;; Source tracking builder creation protocol
  (:export
   #:source-tracking-builder
   #:make-source-tracking-builder))

(cl:in-package  #:text.source-location.source-tracking-builder)

;;; Location construction protocol

(defgeneric make-location (builder bounds)
  (:documentation
   "TODO"))

;;; Helper structure

(defstruct (partial-node
             (:constructor make-partial-node (node location)))
  (node     nil)
  (location nil :read-only t))

;;; The builder

(defclass source-tracking-builder ()
  ((source :initarg  :source
           :accessor source)
   (target :initarg  :target
           :reader   target)
   (callback :initarg :callback
             :reader callback
             :initform nil))
  (:default-initargs
   :source (missing-required-initarg 'source-tracking-builder :source)
   :target (missing-required-initarg 'source-tracking-builder :target)))

(declaim (inline make-source-tracking-builder))
(defun make-source-tracking-builder (source target)
  (make-instance 'source-tracking-builder :source source :target target))

(defmethod make-location ((builder source-tracking-builder)
                          (bounds  null))
  nil)

(defmethod make-location ((builder source-tracking-builder)
                          (bounds  cons))
  (let+ (((start . end) bounds))
    (text.source-location:make-location (source builder) start end)))

(defmethod bp:make-node ((builder source-tracking-builder)
                         (kind    t)
                         &rest initargs &key bounds)
  (let ((node     (apply #'bp:make-node (target builder) kind
                         (remove-from-plist initargs :bounds)))
        (location (make-location builder bounds)))
    (make-partial-node node location)))

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
    (model.transform.trace:recording-transform (() (partial-node-location node)) node1)
    #+later? (when-let ((location (partial-node-location node))
               (callback (callback builder)))
      (funcall callback node1 location))
    node1))
