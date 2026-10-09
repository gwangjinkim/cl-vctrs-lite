;;;; test/package.lisp

(defpackage #:cl-vctrs-lite/test
  (:use #:cl #:fiveam)
  (:import-from #:cl-vctrs-lite
                #:*na* #:na-p #:not-na-p #:na=?
                #:value-type #:common-type
                #:as-col #:col-length #:col-ref #:col-subseq #:col-map #:col-type #:col->list
                #:coerce-value #:col-coerce
                #:recycle-to #:recycle2
                #:v+ #:v- #:v* #:v/ #:v= #:v< #:v> #:v<= #:v>=
                #:v-na-p #:v-not-na-p #:v-not #:v-and #:v-or #:v!=
                #:vec-c)
  (:export #:suite))

(in-package #:cl-vctrs-lite/test)
