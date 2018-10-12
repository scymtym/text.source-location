;;;; cluster.lisp --- Clustering locations.
;;;;
;;;; Copyright (C) 2017-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:text.source-location)

;; TODO turn this into an map- style interface

(defun cluster-locations-by-source (locations &key (key #'identity))
  ;; TODO the hash-table :test may need adjustments when it becomes
  ;; clear what a source can and can't be.
  (let ((key       (ensure-function key))
        (by-source (make-hash-table :test #'equal)))
    (map nil (lambda (location)
               (let ((source (source (funcall key location))))
                 (push location (gethash source by-source '())))) ; TODO using source name is temp hack
         locations)
    by-source))

(defun cluster-locations-by-range
    (locations
     &key
     (key                     #'identity)
     (info                    (unless (emptyp locations)
                                (text-info (content
                                            (source
                                             (funcall key (first-elt locations)))))))
     (intra-cluster-gap-limit 1))
  (let+ ((key               (ensure-function key))
         (current-end-line  nil)
         (current-locations '())
         (clusters          '())
         ((&flet finish-current ()
            (when current-locations
              (push (nreverse current-locations) clusters))))
         ((&flet into-current (end-line location)
            (maxf current-end-line end-line)
            (push location current-locations)))
         ((&flet into-new (end-line location)
            (finish-current)
            (setf current-end-line  end-line
                  current-locations (list location)))))
    (map nil (lambda (location)
               (let+ ((range (range (funcall key location)))
                      ((&values &ign &ign start-line &ign end-line)
                       (line-bounds range info)))
                 (if (and current-end-line
                          (<= (- start-line current-end-line)
                              intra-cluster-gap-limit))
                     (into-current end-line location)
                     (into-new     end-line location))))
         (sort (copy-seq locations) #'location< :key key))
    (finish-current)
    (nreverse clusters)))

(defun cluster-locations (locations
                          &key
                          (key                     #'identity)
                          (intra-cluster-gap-limit 1))
  (let ((result '()))
    (maphash (lambda (source locations)
               (let ((by-range (cluster-locations-by-range
                                locations
                                :key                     key
                                :intra-cluster-gap-limit intra-cluster-gap-limit)))
                 (push (cons source by-range) result)))
             (cluster-locations-by-source locations :key key))
    result))
