;;;; print.lisp --- Printing annotations and locations.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.source-location.print)

;;; Utilities

(defun pretty-line (line) ; TODO rename to user-facing-line
  (1+ line))

(defun pretty-column (column)
  column)

;; TODO test this. especially where pretty is different from number, around 100, etc
(defun line-number-width (number)
  (let ((pretty (pretty-line number)))
    (cond
      ((< pretty 10)    1)
      ((< pretty 100)   2)
      ((< pretty 1000)  3)
      ((< pretty 10000) 4)
      (t                (length (write-to-string pretty :radix nil :base 10))))))

;;; Printing lines

(defvar *style*)

(defun print-source (stream source &key (style *style*))
  (print-source-using-style style stream source))

(defmethod print-annotated-line-using-style (style stream number content annotations
                                             &key
                                             (start-column      0)
                                             end-column
                                             (line-number-width (when number
                                                                  (line-number-width number))))
  (let+ (((&flet print-annotations (position)
            (when-let ((annotations (remove position annotations :test-not #'eq :key #'fourth)))
              (print-line-annotations-using-style
               style stream number position annotations
               :start-column      start-column
               :end-column        end-column
               :line-number-width line-number-width)))))
    (print-annotations :above)
    (print-line-using-style style stream number content
                            :start-column      start-column
                            :end-column        end-column
                            :line-number-width line-number-width)
    (pprint-newline :mandatory stream)
    (print-annotations :below)))

(defun print-annotated-line (stream number content annotations
                             &rest args &key
                             start-column
                             end-column
                             line-number-width
                             (style            *style*))
  (declare (ignore start-column end-column line-number-width))
  (apply #'print-annotated-line-using-style
         style stream number content annotations
         (remove-from-plist args :style)))

;;; For ANNOTATIONS, a sequence of annotations, determine a "bounding
;;; rectangle" represented as start/end line and start/end column. In
;;; addition, compute the start/end index in TEXT and indicate whether
;;; any annotation spans multiple lines.
;;;
;;; ANNOTATIONS are assumed to form a cluster that should be displayed
;;; together.
(defun annotation-bounds (annotations
                          &key
                          (text            (content
                                            (source
                                             (location (first-elt annotations))))) ; TODO ugly
                          (info            (text-info text))
                          (context-lines   2)
                          (context-columns 36))
  (check-type context-lines   non-negative-integer)
  (check-type context-columns non-negative-integer)
  (let ((start*          most-positive-fixnum)
        (start-line*     most-positive-fixnum)
        (start-column*   most-positive-fixnum)
        (end*            0)
        (end-line*       0)
        (end-column*     0)
        (any-multi-line? nil))
    (map nil (lambda (annotation)
               (let+ (((&values start end start-line start-column end-line end-column)
                       (line-bounds (range annotation) info))
                      (multi-line? (/= start-line end-line)))

                 (minf start*        start)
                 (maxf end*          end)

                 (minf start-line*   (max 0 (- start-line context-lines)))
                 (maxf end-line*     (min (length (text.source-location::text-info-newlines info)) ; TODO
                                          (+ end-line context-lines)))

                 (minf start-column* (max 0 (- (if multi-line?
                                                   (min start-column end-column)
                                                   start-column)
                                               context-columns)))
                 (maxf end-column*   (+ (max start-column end-column)
                                        context-columns))

                 (when multi-line?
                   (setf any-multi-line? t))))
         annotations)
    (values start*        end*
            start-line*   end-line*
            start-column* end-column*
            any-multi-line?)))

(defmethod print-annotated-lines-using-style (style stream annotations
                                              &key
                                              (text             (content
                                                                 (source
                                                                  (location (first-elt annotations)))))
                                              (info             (text-info text))
                                              line-number-width
                                              (context-lines    2)
                                              (context-columns  36))
  (let+ (((&values &ign &ign            ; start*        end*
                   start-line*   end-line*
                   start-column* end-column*)
          (annotation-bounds annotations
                             :context-lines   context-lines
                             :context-columns context-columns)))
    (loop :with line-number-width = (or line-number-width (line-number-width end-line*))
          :for  line-number :from start-line* :to end-line*
          :for  (line-start line-end) = (multiple-value-list
                                         (line-bounds line-number info))
          :for  line-length  = (- line-end line-start)
          :for  line-content = (subseq text line-start line-end)
          :do   (print-annotated-line-using-style
                 stream style line-number line-content
                 (loop :for annotation  :in annotations
                       :for range       = (range (location annotation))
                       :for start-index = (index (start range)) ; TODO (bounds range)
                       :for end-index   = (index (end range))
                       :when (<= line-start start-index line-end (1- end-index))
                       :collect (list (- start-index line-start)
                                      (min line-length (- end-index line-start))
                                      annotation
                                      :above)
                       :when (<= (1+ start-index) line-start end-index line-end)
                       :collect (list 0 ; TODO would be good to use same column as start-index
                                      (- end-index line-start)
                                      annotation
                                      :below)
                       :when (<= line-start start-index end-index line-end)
                       :collect (list (- start-index line-start)
                                      (- end-index line-start)
                                      annotation
                                      :below))
                 :start-column      start-column*
                 :end-column        end-column*
                 :line-number-width line-number-width))))

(defun print-annotated-lines (stream annotations &optional colon? at?
                              &rest args &key
                              text
                              info
                              line-number-width
                              context-lines
                              context-columns
                              (style            *style*))
  (declare (ignore colon? at?
                   text info line-number-width context-lines context-columns))
  (apply #'print-annotated-lines-using-style style stream annotations
         (remove-from-plist args :style)))

(defmethod print-annotations-using-style (style stream annotations &key (context-lines 2))
  (let ((clusters (cluster-locations
                   annotations
                   :key                     #'location
                   :intra-cluster-gap-limit (+ 2 (* 2 context-lines)))))
    (map nil (lambda+ ((source . annotations))
               (print-source-using-style style stream source)
               (format stream "~@:_~2@T")
               (pprint-logical-block (stream annotations)
                 (loop :with end-location = (extremum (map 'list #'location (lastcar annotations))
                                                      (complement #'location<)
                                                      :key #'end)
                       :with text-info = (text-info (content (source end-location)))
                       :with line-number-width
                          = (line-number-width
                             (+ (nth-value
                                 4 (line-bounds (range end-location) text-info))
                                context-lines))
                       :for  (annotation next) :on annotations
                       :do   (print-annotated-lines-using-style
                              stream style annotation
                              :line-number-width line-number-width
                              :context-lines     context-lines)
                       :when next
                       :do   (format stream "~@:_~V<⁞~> ⁞  ⁞~@:_~@:_"
                                     line-number-width)))
               (pprint-newline :mandatory stream))
         clusters)
    (force-output *trace-output*)))

(defun print-annotations (stream annotations
                          &rest args &key
                          context-lines
                          (style        *style*))
  (declare (ignore context-lines))
  (apply #'print-annotations-using-style style stream annotations
         (remove-from-plist args :context-lines)))
