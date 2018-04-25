(cl:defpackage #:text.source-location.lookup
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:local-nicknames
   (#:sloc #:text.source-location))

  ;;
  (:export
   #:location-in?)  ; TODO add to text.s-loc package?

  ;; Lookup protocol
  (:export
   #:lookup
   #:in)

  ;; Index mutation protocol
  (:export
   #:add!
   #:remove!)

  ;; Index creation
  (:export
   #:make-index))
