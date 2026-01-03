;;;; src/protocol.lisp

(in-package #:cl-vctrs-lite)

(defun as-col (x)
  "Normalize list -> simple-vector; vector -> itself.
Strings are not treated as vectors."
  (cond
    ;; list -> simple-vector
    ((and (listp x) (not (stringp x)))
     (coerce x 'simple-vector))
    ;; arrays (non-strings) are already vector-like
    ((and (arrayp x) (not (stringp x))) x)
    (t (%simple-error "as-col: not a vector-like column: ~s" x))))

(defun col-length (col)
  "Length of a vector-like column."
  (cond
    ((and (arrayp col) (not (stringp col))) (length col))
    ((and (listp col) (not (stringp col))) (length col))
    (t (%simple-error "col-length: not a vector-like column: ~s" col))))

(defun col-ref (col i)
  "0-based indexing access with bounds checking and informative errors."
  (unless (and (integerp i) (<= 0 i))
    (%simple-error "col-ref: index must be a non-negative integer, got ~s" i))
  (let* ((n (col-length col)))
    (if (<= (1+ i) n)
        (cond
          ((and (arrayp col) (not (stringp col))) (aref col i))
          ((and (listp col) (not (stringp col))) (nth i col))
          (t (%simple-error "col-ref: not a vector-like column: ~s" col)))
        (%simple-error "col-ref: index ~a out of range for length ~a" i n))))

(defun col->list (col)
  "Convert column to list (for tests/debug)."
  (cond
    ((and (listp col) (not (stringp col))) col)
    ((and (arrayp col) (not (stringp col)))
     (let ((n (col-length col)))
       (loop for i from 0 below n collect (col-ref col i))))
    (t (%simple-error "col->list: not a vector-like column: ~s" col))))

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
