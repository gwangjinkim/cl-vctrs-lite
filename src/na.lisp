;;;; src/na.lisp

(in-package #:cl-vctrs-lite)

(defstruct (na
            (:constructor %make-na ())
            (:predicate na-p))
  "Singleton missing value object.")

(defparameter *na* (%make-na)
  "Singleton missing value.")

(defmethod print-object ((x na) stream)
  (declare (ignore x))
  (princ "NA" stream))

(defun not-na-p (x)
  (not (na-p x)))

(defun na=? (a b)
  "NA-propagating equality:
- if either is NA -> NA
- otherwise -> EQL"
  (if (or (na-p a) (na-p b))
      *na*
      (eql a b)))
