(cl:in-package #:text.source-location.print)

;;; Annotation printing protocol

(defgeneric print-annotations-using-style (style stream annotations &key context-lines))

(defgeneric print-source-using-style (style stream source))

(defgeneric print-annotated-lines-using-style (style stream annotations
                                               &key
                                               text
                                               info
                                               line-number-width
                                               context-lines
                                               context-columns))

(defgeneric print-annotated-line-using-style (style stream number content annotations
                                              &key
                                              start-column end-column
                                              line-number-width))

(defgeneric print-line-using-style (style stream number content
                                    &key
                                    start-column end-column
                                    line-number-width))

(defgeneric print-line-annotations-using-style (style stream line-number position annotations
                                                &key
                                                start-column end-column
                                                line-number-width))

(defgeneric print-line-annotation-using-style (style stream width position annotation))

;;; Service

(service-provider:define-service style)

(defun make-style (designator &rest args)
  (apply #'service-provider:make-provider 'style designator args))

(define-compiler-macro make-style (designator &rest args)
  `(service-provider:make-provider 'style ,designator ,@args))
