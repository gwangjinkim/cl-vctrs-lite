;;;; src/util.lisp

(in-package #:cl-vctrs-lite)

(defun %vector-like-p (x)
  "v0.1: vector-like means a proper list OR an array.
Strings are treated as scalar values (not vector-like)."
  (or (and (listp x) (not (stringp x)))
      (and (arrayp x) (not (stringp x)))))

(defun %ensure-nonnegative-integer (n &key (name "n"))
  (unless (and (integerp n) (<= 0 n))
    (error "~a must be a non-negative integer, got: ~s" name n))
  n)

(defun %simple-error (fmt &rest args)
  (error (apply #'format nil fmt args)))
