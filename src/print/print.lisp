;;;; print.lisp --- Printing annotations and locations.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.source-location.print)

;;; Utilities

(defun pretty-line (line)
  (1+ line))

(defun pretty-column (column)
  column)

(defun line-number-width (number)
  (cond
    ((< number 10)    1)
    ((< number 100)   2)
    ((< number 1000)  3)
    ((< number 10000) 4)
    (t                (length (write-to-string number :radix nil :base 10)))))

;;; Printing lines

(defvar *style*)

(defun print-source (stream source &key (style *style*))
  (print-source-using-style style stream source))

(defun print-annotated-line (stream number content annotations
                             &key
                             (start-column      0)
                             end-column
                             (line-number-width (when number
                                                  (line-number-width number)))
                             (style             *style*))
  (print-line-using-style style stream number content
                          :start-column      start-column
                          :end-column        end-column
                          :line-number-width line-number-width)
  (unless (emptyp annotations)
    (print-line-annotations-using-style
     style stream number annotations
     :start-column      start-column
     :end-column        end-column
     :line-number-width line-number-width)))

(defun annotation-bounds (annotations
                          &key
                          (text          (content
                                          (source
                                           (location (first-elt annotations)))))
                          (info          (text-info text))
                          (context-lines 2))
  (let ((start*          most-positive-fixnum)
        (start-line*     most-positive-fixnum)
        (start-column*   most-positive-fixnum)
        (end*            0)
        (end-line*       0)
        (end-column*     0)
        (any-multi-line? nil))
    (map nil (lambda (annotation)
               (let+ (((&values start end start-line start-column end-line end-column)
                       (line-bounds (range annotation) info)))
                 (minf start*        start)
                 (minf start-line*   (max 0 (- start-line context-lines)))
                 (minf start-column* start-column)
                 (maxf end*          end)
                 (maxf end-line*     (min (length (text.source-location::text-info-newlines info)) ; TODO
                                          (+ end-line context-lines)))
                 (maxf end-column*   end-column)
                 (when (/= start-line end-line)
                   (setf any-multi-line? t))))
         annotations)
    (values start*        end*
            start-line*   end-line*
            start-column* end-column*
            any-multi-line?)))

(defun print-annotated-lines (stream annotations &optional colon? at?
                              &key
                              (text             (content
                                                 (source
                                                  (location (first-elt annotations)))))
                              (info             (text-info text))
                              line-number-width
                              (context-lines    2)
                              (context-columns  36))
  (declare (ignore colon? at?))
  (let+ (((&values ; text info
                   &ign &ign ; start*        end*
                   start-line*   end-line*
                   start-column* end-column*
                   any-multi-line?)
          (annotation-bounds annotations :context-lines context-lines)))
    #+no (map nil (lambda (annotation)
               (let+ (((&values start end start-line start-column end-line end-column)
                       (line-bounds (range annotation) info)))
                 (minf start*        start)
                 (minf start-line*   (max 0 (- start-line context-lines)))
                 (minf start-column* start-column)
                 (maxf end*          end)
                 (maxf end-line*     (min (length (text.source-location::text-info-newlines info)) ; TODO
                                          (+ end-line context-lines)))
                 (maxf end-column*   end-column)
                 (when (/= start-line end-line)
                   (setf any-multi-line? t))))
         annotations)
    (loop :with line-number-width = (or line-number-width (line-number-width end-line*))
          :for  line-number :from start-line* :to end-line*
          :for  (line-start line-end) = (multiple-value-list
                                         (line-bounds line-number info))
          :for  line-content = (subseq text line-start line-end)
          :do   (print-annotated-line
                 stream line-number line-content
                 (loop :for annotation  :in annotations
                       :for range       = (range (location annotation))
                       :for start-index = (index (start range)) ; TODO (bounds range)
                       :for end-index   = (index (end range))
                       :when (<= line-start start-index end-index line-end)
                       :collect (list (- start-index line-start)
                                      (- end-index line-start)
                                      (text annotation)))
                 :start-column      (max 0 (- start-column* context-columns))
                 :end-column        (+ end-column* context-columns)
                 :line-number-width line-number-width)
                (pprint-newline :mandatory stream))))

(defun print-annotations (stream annotations &key (context-lines 2))
  (let* ((clusters (cluster-locations
                    annotations
                    :key                     #'location
                    :intra-cluster-gap-limit (+ 2 (* 2 context-lines)))))
    (map nil (lambda+ ((source . annotations))
               (print-source stream source)
               (format stream "~@:_~2@T")
               (pprint-logical-block (stream annotations)
                 (loop :with line-number-width = 2
                       :for (annotation next) :on annotations
                       :do (print-annotated-lines
                            stream annotation nil nil
                            :line-number-width line-number-width
                            :context-lines     context-lines)
                       :when next
                       :do (format stream "~@:_~V<⁞~> ⁞  ⁞~@:_~@:_"
                                   line-number-width)))
               (pprint-newline :mandatory stream))
         clusters)))
