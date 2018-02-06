(cl:in-package #:text.source-location.print)

(defclass style-unicode ()
  ())

(defmethod print-source-using-style ((style  style-unicode)
                                     (stream t)
                                     (source t))
  (format stream "In ~A:" source))

(defmethod print-line-using-style ((style   style-unicode)
                                   (stream  t)
                                   (number  integer)
                                   (content string)
                                   &key
                                   (start-column      0)
                                   end-column
                                   (line-number-width (line-number-width number)))
  (let* ((end-column (when (and end-column (< end-column (length content)))
                       end-column))
         (slice      (if (< start-column (length content))
                         (subseq content start-column end-column)
                         "")))
    (format stream "~@[~V:D │ ~]~
                    ~[~:;…~]~A~:[~;…~]"
            line-number-width (pretty-line number)
            start-column slice end-column)))

(defmethod print-line-annotation-using-style ((style      style-unicode)
                                              (stream     t)
                                              (width      integer)
                                              (annotation t))
  (format stream "~V,,,'▔<~>~A" width annotation))

(defmethod print-line-annotations-using-style
    ((style       style-unicode)
     (stream      t)
     (line-number integer)
     (annotations cons)
     &key
     (start-column      0)
     end-column
     (line-number-width (line-number-width line-number)))
  (declare (ignore end-column))
  (let+ (((&flet fringe ()
            (format stream "~@:_~@[~V@T │ ~]" line-number-width))))
    (fringe)
    (loop :for previous = (max 0 (1- start-column)) :then (+ end (length text))
          :for (start end text) :in annotations
          :when (> previous start)
          :do (fringe)
              (setf previous (max 0 (1- start-column)))
          :do (format stream "~V@T" (- start previous))
              (print-line-annotation-using-style
               style stream (- end start) text))))

;;; `style-unicode+ansi-escapes'

(defclass style-unicode+ansi-escapes (style-unicode)
  ())

(defmethod print-source-using-style ((style  style-unicode+ansi-escapes)
                                     (stream t)
                                     (source t))
  (format stream "In [35m~A[0m:" source))

(defmethod print-line-annotation-using-style
    :around ((style      style-unicode+ansi-escapes)
             (stream     t)
             (width      integer)
             (annotation t))
  (write-string (ecase (random-elt '(:error :warning :note))
                  ((nil)    "")
                  (:error   "[31m")
                  (:warning "[33m")
                  (:note    "[32m"))
                stream)
  (unwind-protect
       (call-method)
    (write-string "[0m" stream)))

(setf *style* (make-instance 'style-unicode+ansi-escapes))
