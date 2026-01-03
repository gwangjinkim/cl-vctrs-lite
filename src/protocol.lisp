;;;; src/protocol.lisp

(in-package #:cl-vctrs-lite)

(defun as-col (x)
  "Normalize list -> simple-vector; vector -> itself. (stub for M3)"
  (declare (ignore x))
  (%simple-error "as-col: not implemented yet"))

(defun col-length (col)
  "Length of a vector-like column. (stub for M3)"
  (declare (ignore col))
  (%simple-error "col-length: not implemented yet"))

(defun col-ref (col i)
  "0-based indexing access. (stub for M3)"
  (declare (ignore col i))
  (%simple-error "col-ref: not implemented yet"))

(defun col->list (col)
  "Convert column to list (for tests/debug). (stub for M3)"
  (declare (ignore col))
  (%simple-error "col->list: not implemented yet"))

(defun col-subseq (col indices)
  "Select indices (list/vector of ints) from col. (stub for M4)"
  (declare (ignore col indices))
  (%simple-error "col-subseq: not implemented yet"))

(defun col-map (fn col)
  "Map FN over COL and return a column. (stub for M4)"
  (declare (ignore fn col))
  (%simple-error "col-map: not implemented yet"))

(defun col-type (col)
  "Type tag for column (ignore NA). (stub for M4)"
  (declare (ignore col))
  :any)
