(in-package #:cl-vctrs-lite)

(defun coerce-value (x target-type &key (on-error :error))
  "Coerce a single value. (stub for M5)"
  (declare (ignore x target-type on-error))
  (%simple-error "coerce-value: not implemented yet"))

(defun col-coerce (col target-type &key (on-error :error))
  "Coerce an entire column. (stub for M5)"
  (declare (ignore col target-type on-error))
  (%simple-error "col-coerce: not implemented yet"))
