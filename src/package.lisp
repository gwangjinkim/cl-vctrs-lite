;;;; src/package.lisp

(defpackage #:cl-vctrs-lite
  (:use #:cl)
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
   #:v+ #:v- #:v* #:v/ #:v= #:v< #:v> #:v<= #:v>=))

(defpackage #:cl-vctrs-lite/test
  (:use #:cl #:fiveam)
  (:import-from #:cl-vctrs-lite
                #:*na* #:na-p #:not-na-p #:na=?
                #:as-col #:col-length #:col-ref #:col-subseq #:col-map #:col-type #:col->list
                #:coerce-value #:col-coerce
                #:recycle-to #:recycle2
                #:v+ #:v- #:v* #:v/ #:v= #:v< #:v> #:v<= #:v>=)
  (:export #:suite))
