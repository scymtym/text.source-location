(cl:in-package #:text.source-location.lookup.test)

(in-suite :text.source-location.lookup)

(test range-index.smoke
  "Smoke test for the `lookup' generic function."

  (let* ((index     (make-range-index))
         (source    "foobar")
         (location1 (make-location source 0 5))
         (location2 (make-location source 1 3))
         (location3 (make-location source 3 5)))
    (add! location1 index)
    (add! location2 index)
    (add! location3 index)
    (is (equal (list location1) (lookup (make-range 1 3) index)))
    (is (set-equal (list location2 location1)
                   (lookup (make-instance 'text.source-location::index-position :index 1) index)))))
