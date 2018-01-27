(cl:in-package #:text.source-location)

(make-instance 'annotation :location (make-instance 'location :range (make-range 10 15))
               :text "declared here")

(defvar *location*
  (make-instance 'location :source-content "foo bar
baz fez
whoop"
                 :range (make-range (make-instance 'index-position :index 10)
                                    (make-instance 'index-position :index 15))))

(lines (range *location*) (source-content *location*))

#+later (let* ((filename "~/code/citec/citk/recipes-next/distributions/bart-demo.distribution")
       (source (read-file-into-string filename))
       (clusters
        (cluster-locations-by-range (list #+no(make-instance 'location
                                                             :source-content source
                                                             :range          (make-range 2 3))
                                          #+no (make-instance 'location
                                                              :source-content source
                                                              :range          (make-range 4 7))
                                          (make-instance 'location
                                                         :source-content source
                                                         :range          (make-range 13 27))
                                          (make-instance 'location
                                                         :source-content source
                                                         :range          (make-range 38 39))
                                          (make-instance 'location
                                                         :source-content source
                                                         :range          (make-range 232 237)))
                                    :intra-cluster-gap-limit 1)))
  (print (length source))
  (let ((stream *error-output*))
    (pprint-logical-block (stream nil)
      (format stream "~V,,,'─<~>~@:_" 80)
      (format stream "In ~A:~@:_~2@T" filename)
      (pprint-logical-block (stream clusters)
        (format stream "~{~/text.source-location::print-annotated-lines/~^~@:_⁞ ⁞  ⁞~@:_~@:_~}"
                clusters)))))








#+test (with-output-to-string (stream)
  (pprint-logical-block (stream nil)
    (print-annotated-lines stream (list (make-instance 'location
                                                       :source-content (format nil "foo~%foo bar() faz~%baz")
                                                       :range          (make-range 8 11))))))


#+test (with-output-to-string (stream)
  (pprint-logical-block (stream nil)
    (print-annotated-line stream 100 "foo baz baz"
                          '((0 3 "foo bar")
                            (4 7 "fez dip")))))
