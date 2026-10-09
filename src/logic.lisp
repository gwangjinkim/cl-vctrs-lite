;;;; src/logic.lisp

(in-package #:cl-vctrs-lite)

;; Vectorized missingness and three-valued (Kleene) logic.
;; Logical values are T / NIL; *na* is "unknown". This is the same
;; convention R uses for logical vectors, so NA never silently turns
;; into TRUE or FALSE.

(defun v-na-p (x)
  "Vectorized NA test: a simple-vector of T/NIL, never NA."
  (col-map (lambda (e) (if (na-p e) t nil)) (as-col x)))

(defun v-not-na-p (x)
  "Vectorized negation of V-NA-P."
  (col-map (lambda (e) (if (na-p e) nil t)) (as-col x)))

(defun %logical-cell (x name)
  (cond ((na-p x) *na*)
        ((or (eq x t) (null x)) x)
        (t (%simple-error "~a expected logical (T, NIL or NA), got ~s" name x))))

(defun v-not (a)
  "Vectorized NOT; NA stays NA."
  (col-map (lambda (x)
             (let ((x (%logical-cell x "v-not:")))
               (if (na-p x) *na* (not x))))
           (as-col a)))

(defun %and2 (a b)
  (multiple-value-bind (va vb n) (recycle2 a b)
    (let ((out (make-array n)))
      (dotimes (i n out)
        (let ((x (%logical-cell (aref va i) "v-and:"))
              (y (%logical-cell (aref vb i) "v-and:")))
          (setf (aref out i)
                (cond ((or (eq x nil) (eq y nil)) nil) ; FALSE dominates NA
                      ((or (na-p x) (na-p y)) *na*)
                      (t t))))))))

(defun %or2 (a b)
  (multiple-value-bind (va vb n) (recycle2 a b)
    (let ((out (make-array n)))
      (dotimes (i n out)
        (let ((x (%logical-cell (aref va i) "v-or:"))
              (y (%logical-cell (aref vb i) "v-or:")))
          (setf (aref out i)
                (cond ((or (eq x t) (eq y t)) t)       ; TRUE dominates NA
                      ((or (na-p x) (na-p y)) *na*)
                      (t nil))))))))

(defun v-and (&rest args)
  "Vectorized, recycling, NA-aware AND of one or more logical vectors."
  (if (null args) (vector t) (reduce #'%and2 args)))

(defun v-or (&rest args)
  "Vectorized, recycling, NA-aware OR of one or more logical vectors."
  (if (null args) (vector nil) (reduce #'%or2 args)))

(defun v!= (a b)
  "Vectorized inequality; NA-propagating like V=."
  (v-not (v= a b)))
