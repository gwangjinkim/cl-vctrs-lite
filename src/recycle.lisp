;;;; src/recycle.lisp

(in-package #:cl-vctrs-lite)

(defun recycle-to (col n &key (name "value"))
  "Recycle COL to length N using strict rules. (stub for M6)"
  (declare (ignore col n name))
  (%simple-error "recycle-to: not implemented yet"))

(defun recycle2 (a b &key (name-a "a") (name-b "b"))
  "Recycle A and B to a common size. (stub for M6)"
  (declare (ignore a b name-a name-b))
  (%simple-error "recycle2: not implemented yet"))
