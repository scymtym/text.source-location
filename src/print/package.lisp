(cl:defpackage #:text.source-location.print
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:text.source-location)

  ;; Protocol
  (:export
   #:print-annotations-using-style

   #:print-source-using-style

   #:print-annotated-lines-using-style

   #:print-annotated-line-using-style

   #:print-line-using-style

   #:print-line-annotation-using-style
   #:print-line-annotations-using-style

   #:print-annotations)

  ;; Service
  (:export
   #:make-style))
