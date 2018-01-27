;;;; print.lisp --- Printing annotations and locations.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.source-location)

;;; Utilities

(defun pretty-line (line)
  (1+ line))

(defun pretty-column (column)
  column)

(defun line-number-width (number)       ; TODO table for small numbers
  (length (with-output-to-string (stream)
            (format stream "~:D" number))))

;;; Printing lines

(defun print-annotated-line (stream number content annotations
                             &key
                             (start-column 0)
                             end-column
                             (line-number-width (when number
                                                  (line-number-width number))))
  (let* ((end-column (when (and end-column (< end-column (length content)))
                       end-column))
         (slice      (if (< start-column (length content))
                         (subseq content start-column end-column)
                         "")))
    (format stream "~@[~V:D │ ~]~
                    ~[~:;…~]~A~:[~;…~]"
            line-number-width (pretty-line number)
            start-column slice end-column))
  (when annotations
    (format stream "~@:_~@[~V@T │ ~]" line-number-width)
    (loop :for previous = (max 0 (1- start-column)) :then (+ end (length text))
       :for (start end text) :in annotations
       :when (> previous start)
       :do (format stream "~@:_~@[~V@T │ ~]" line-number-width)
           (setf previous (max 0 (1- start-column)))
       :do (format stream (ecase (random-elt '(:error :warning :note))
                            ((nil)    "~V@T~V,,,'▔<~>~A")
                            (:error   "~V@T[31m~V,,,'▔<~>~A[0m")
                            (:warning "~V@T[33m~V,,,'▔<~>~A[0m")
                            (:note    "~V@T[32m~V,,,'▔<~>~A[0m"))
                   (- start previous) (- end start) text))))

(defun print-annotated-lines (stream locations &optional colon? at?)
  (declare (ignore colon? at?))
  (let+ ((context-lines   2)  ; TODO should be parameters
         (context-columns 16)

         (text            (source-content (location (first-elt locations))))
         (info            (text-info text))
         (start*          most-positive-fixnum)
         (start-line*     most-positive-fixnum)
         (start-column*   most-positive-fixnum)
         (end*            0)
         (end-line*       0)
         (end-column*     0)
         (any-multi-line? nil))
    (map nil (lambda (location)
               (let+ (((&values start end start-line start-column end-line end-column)
                       (line-bounds (range (location location)) info)))
                 #+no (format *terminal-io* "~D-~D ~D:~D-~D:~D~%"
                         start end
                         start-line start-column
                         end-line   end-column)
                 (minf start*        start)
                 (minf start-line*   (max 0 (- start-line context-lines)))
                 (minf start-column* start-column)
                 (maxf end*          end)
                 (maxf end-line*     (min (length (text-info-newlines info)) (+ end-line context-lines)))
                 (maxf end-column*   end-column)
                 (when (/= start-line end-line)
                   (setf any-multi-line? t))))
         locations)
    (let+ ((start-in-first-line (- start* (line-bounds start-line* info)))
           (end-in-last-line    (- (nth-value 1 (line-bounds end-line* info)) end*)))

      (loop :with line-number-width = (line-number-width end-line*)
            :for line-number :from start-line* :to end-line*
            :for (line-start line-end) = (multiple-value-list
                                          (line-bounds line-number info))
            :for line-content = (subseq text line-start line-end)
            :do (print-annotated-line
                 stream line-number line-content
                 (loop :for annotation :in locations
                    :for location = (location annotation)
                    :when (<= line-start
                              (index (start (range location)))
                              (index (end   (range location)))
                              line-end)
                    :collect (list (- (index (start (range location))) line-start)
                                   (- (index (end   (range location))) line-start)
                                   (text annotation)))
                 :start-column      (max 0 (- start-column* context-columns))
                 :end-column        (+ end-column* context-columns)
                 :line-number-width line-number-width)
                (pprint-newline :mandatory stream)))))

(defun print-annotations (stream annotations)
  (let* ((clusters (cluster-locations
                    annotations
                    :key                     #'location
                    :intra-cluster-gap-limit 1)))
    (map nil (lambda+ ((source . annotations))
               (format stream "In ~A:~@:_~2@T" source)
               (pprint-logical-block (stream annotations)
                 (format stream "~{~
                                   ~/text.source-location::print-annotated-lines/~
                                   ~^~@:_⁞ ⁞  ⁞~@:_~@:_~
                                 ~}"
                         annotations)))
         clusters)))
