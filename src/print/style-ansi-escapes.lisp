(cl:in-package #:text.source-location.print)

(defclass style-unicode ()
  ())

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

(defmethod print-annotations-using-style ((style       style-unicode)
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
          :do (format stream (ecase (random-elt '(:error :warning :note))
                               ((nil)    "~V@T~V,,,'▔<~>~A")
                               (:error   "~V@T[31m~V,,,'▔<~>~A[0m")
                               (:warning "~V@T[33m~V,,,'▔<~>~A[0m")
                               (:note    "~V@T[32m~V,,,'▔<~>~A[0m"))
                      (- start previous) (- end start) text))))

()

(defclass style-unicode+ansi-escapes ()
  ())
