(cl:in-package #:text.source-location.test)

(test smoke
  "Smoke test"

  (model.transform.trace:with-tracer ((model.transform.trace:make-tracer))
    (list
     (architecture.builder-protocol:with-builder ((make-source-tracking-builder :input 'list))
       (model.transform.trace:with-transform (:test)
         (architecture.builder-protocol:node* (:foo :bar 1 :bounds (cons 1 2))
           (1 :baz (architecture.builder-protocol:node* (:fez :bounds (cons 3 4)))))))
     model.transform.trace::*tracer*)))

#+test (let ((tracker (make-instance 'language.toy.source-tracking::tracker)))
          (list
           (language.toy.source-tracking:with-tracker (tracker)
             (architecture.builder-protocol:with-builder ((make-instance 'source-tracking-builder :target 'list))
               (node* (:foo :bar 1 :bounds (cons 1 2))
                 (1 :baz (node* (:fez :bounds (cons 3 4)))))))
           tracker))
