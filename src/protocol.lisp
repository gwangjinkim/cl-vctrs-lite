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
  "Select elements of COL at given zero-based INDICES (list or vector).
Returns a simple-vector. Relies on col-ref for bounds checking."
  (let* ((idx-list (cond
                     ((and (arrayp indices) (not (stringp indices)))
                      (coerce indices 'list))
                     ((and (listp indices) (not (stringp indices)))
                      indices)
                     (t (%simple-error "col-subseq: indices must be list or vector, got: ~s" indices))))
         (k (length idx-list))
         (out (make-array k)))
    (loop for idx in idx-list
          for j from 0
          do (setf (aref out j) (col-ref col idx)))
    out))

(defun col-map (fn col)
  "Map FN over COL and return a simple-vector (v0.1)."
  (let* ((n (col-length col))
         (out (make-array n)))
    (dotimes (i n)
      (setf (aref out i) (funcall fn (col-ref col i))))
    out))

(defun col-type (col)
  "Type tag for COL, ignoring NA values. Uses value-type/common-type.
Returns :any if no non-NA values are present."
  (let* ((n (col-length col))
         (acc nil))
    (dotimes (i n)
      (let* ((x (col-ref col i))
             (tx (value-type x)))
        (unless (eq tx :na)
          (setf acc (if acc (common-type acc tx) tx)))))
    (or acc :any)))
