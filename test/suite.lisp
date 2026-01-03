;;;; test/suite.lisp

(in-package #:cl-vctrs-lite/test)

(def-suite suite
  :description "cl-vctrs-lite test suite.")

(in-suite suite)

;; Shared fixtures (some will be used once protocol exists)
(defparameter *v-int* #(1 2 3))
(defparameter *v-dbl* #(1.0 2.0 3.0))
(defparameter *v-mix* (vector 1 cl-vctrs-lite:*na* 3))
(defparameter *v-str* #("a" "b" "c"))
