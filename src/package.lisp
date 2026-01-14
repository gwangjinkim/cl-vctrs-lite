;;;; src/package.lisp

(defpackage #:cl-vctrs-lite
  (:use #:cl)
  (:nicknames #:vctrs)
  (:export
   ;; NA
   #:*na* #:na-p #:not-na-p #:na=?

   ;; Types
   #:value-type #:common-type

   ;; Protocol
   #:as-col #:col-length #:col-ref #:col-subseq #:col-map #:col-type
   #:coerce-value #:col-coerce
   #:col->list

   ;; Recycling
   #:recycle-to #:recycle2

   ;; Ops
   #:v+ #:v- #:v* #:v/ #:v= #:v< #:v> #:v<= #:v>=

   ;; Concatenation
   #:vec-c))
