(cl:defpackage #:text.source-location.lookup
  (:use
   #:cl
   #:let-plus)

  (:local-nicknames
   (#:sloc #:text.source-location))

  ;;
  (:export
   #:location-in?)  ; TODO add to text.s-loc package?

  ;; Lookup protocol
  (:export
   #:lookup)

  ;; Index mutation protocol
  (:export
   #:add!
   #:remove!)

  ;; Index creation
  (:export
   #:make-index))
