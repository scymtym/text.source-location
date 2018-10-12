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

   #:make-location
   #:record-location)

  ;; Source tracking builder creation protocol
  (:export
   #:source-tracking-builder
   #:make-source-tracking-builder))

(cl:in-package  #:text.source-location.source-tracking-builder)

;;; Location construction protocol

(defgeneric make-location (builder bounds)
  (:documentation
   "TODO"))

(defgeneric record-location (builder node location)
  (:documentation
   "TODO"))

;;; Helper structure

(defstruct (partial-node
            (:constructor make-partial-node (node location)))
  (node     nil)
  (location nil :read-only t))

;;;

(defclass %source-tracking-builder (bp:forwarding-mixin) ; TODO rename this one to source-tracking-builder
  ((source :initarg  :source
           :accessor source))
  (:default-initargs
   :source (missing-required-initarg '%source-tracking-builder :source)))

(defmethod make-location ((builder %source-tracking-builder)
                          (bounds  null))
  nil)

(defmethod make-location ((builder %source-tracking-builder)
                          (bounds  cons))
  (let+ (((start . end) bounds))
    (text.source-location:make-location (source builder) start end)))

(defmethod bp:make-node ((builder %source-tracking-builder)
                         (kind    t)
                         &rest initargs &key bounds)
  (let ((node     (apply #'call-next-method builder kind
                         (remove-from-plist initargs :bounds)))
        (location (make-location builder bounds)))
    (make-partial-node node location)))

(defmethod bp:finish-node ((builder %source-tracking-builder)
                           (kind    t)
                           (node    partial-node))
  (let+ (; ((&structure partial-node- node location) node)
         (node1 (bp:finish-node (bp:target builder) kind (partial-node-node node))))
    (record-location builder node1 (partial-node-location node))
    node1))

(defmethod bp:relate ((builder  %source-tracking-builder)
                      (relation t)
                      (left     partial-node)
                      (right    t)
                      &rest initargs &key)
  (let+ (((&structure partial-node- node) left))
    (setf node (apply #'bp:relate (bp:target builder) relation node right
                      initargs))
    left))

;;; The builder

(defclass source-tracking-builder (%source-tracking-builder)
  ())

(declaim (inline make-source-tracking-builder))
(defun make-source-tracking-builder (source target)
  (make-instance 'source-tracking-builder :source source :target target))

(defmethod record-location ((builder  source-tracking-builder)
                            (node     t)
                            (location t))
  (model.transform.trace:recording-transform (() location) node))

;;; `callback-source-tracking-builder'

(defclass callback-source-tracking-builder (%source-tracking-builder)
  ((callback :initarg  :callback
             :reader   callback
             :initform nil)))

(defmethod record-location ((builder  callback-source-tracking-builder)
                            (node     t)
                            (location t))
  (when-let ((location location)
             (callback (callback builder)))
    (funcall callback node location)))
