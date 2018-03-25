(cl:in-package #:text.source-location.lookup.test)

(in-suite :text.source-location.lookup)

(test index.smoke
  "Smoke test for the `lookup' generic function."
  
  (let ((i (make-instance 'index)))
    (add! (sloc:make-location #1="foobar" 0 5) i)
    (add! (sloc:make-location #1# 1 3) i)
    (add! (sloc:make-location #1# 3 5) i)
    (values (lookup (sloc:make-range 1 3) i)
            (lookup (make-instance 'sloc::index-position :index 1) i))))
