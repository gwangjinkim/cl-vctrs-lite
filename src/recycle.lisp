;;;; src/recycle.lisp

(in-package #:cl-vctrs-lite)

(defun broadcast-scalar (x n)
  "Return a simple-vector of length N filled with X."
  (let ((out (make-array n)))
    (dotimes (i n) (setf (aref out i) x))
    out))

(defun recycle-to (col n &key (name "value"))
  "Recycle COL to length N using strict rules.
Scalar means non-vector-like or vector-like of length 1.
Returns a simple-vector."
  (%ensure-nonnegative-integer n :name "n")
  (let* ((vec-like (%vector-like-p col))
         (len (if vec-like (col-length col) 1)))
    (cond
      ;; already desired length
      ((= len n)
       (if vec-like (as-col col) (broadcast-scalar col n)))
      ;; length-1 broadcast (including scalar)
      ((= len 1)
       (let ((val (if vec-like (col-ref (as-col col) 0) col)))
         (broadcast-scalar val n)))
      (t
       (%simple-error "recycle-to: cannot recycle ~a of length ~a to length ~a"
                      name len n)))))

(defun recycle2 (a b &key (name-a "a") (name-b "b"))
  "Recycle A and B to a common size with strict rules.
Returns three values (A2 B2 N)."
  (let* ((va (%vector-like-p a))
         (vb (%vector-like-p b))
         (na (if va (col-length a) 1))
         (nb (if vb (col-length b) 1)))
    (when (and (> na 1) (> nb 1) (/= na nb))
      (%simple-error "recycle2: incompatible lengths ~a and ~a" na nb))
    (let* ((n (max na nb))
           (a2 (recycle-to a n :name name-a))
           (b2 (recycle-to b n :name name-b)))
      (values a2 b2 n))))
