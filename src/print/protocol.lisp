(cl:in-package #:text.source-location.print)

(defgeneric print-line-using-style (style stream number content &key start-column end-column line-number-width))

(defgeneric print-line-annotation-using-style (style stream width position annotation))

(defgeneric print-line-annotations-using-style (style stream line-number position annotations &key start-column end-column line-number-width))

;;; Service

(service-provider:define-service style)
