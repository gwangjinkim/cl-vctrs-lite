(in-package #:cl-vctrs-lite)

(defun coerce-value (x target-type &key (on-error :error))
  "Coerce a single value X to TARGET-TYPE (keyword).
on-error modes:
  :error -> signal error on failure
  :na    -> return *na* on failure
  :keep  -> return X unchanged on failure"
  (labels ((fail (fmt &rest args)
             (ecase on-error
               (:error (apply #'%simple-error fmt args))
               (:na *na*)
               (:keep x))))
    ;; NA propagates for any target type
    (when (na-p x) (return-from coerce-value *na*))
    (ecase target-type
      (:string
       ;; Strings: just return existing string; others via princ-to-string
       (if (stringp x) x (princ-to-string x)))
      (:double
       (cond
         ((floatp x) x)
         ((integerp x) (coerce x 'double-float))
         ;; v0.1: reject strings even if parseable
         ((stringp x) (fail "coerce-value: cannot coerce string ~s to :double" x))
         (t (fail "coerce-value: unsupported coercion ~s -> :double" x))))
      (:int
       (cond
         ((integerp x) x)
         ((floatp x)
          (multiple-value-bind (q r) (truncate x)
            (if (zerop r)
                q
                (fail "coerce-value: non-integer float ~s cannot coerce to :int" x))))
         (t (fail "coerce-value: unsupported coercion ~s -> :int" x))))
      (:bool
       (cond
         ((or (eq x t) (null x)) x)
         (t (fail "coerce-value: unsupported coercion ~s -> :bool" x))))
      (:any x))))

(defun col-coerce (col target-type &key (on-error :error))
  "Coerce an entire column COL to TARGET-TYPE. Returns a simple-vector.
Accepts lists or vectors as input. Honors on-error as in coerce-value."
  (let* ((v (as-col col))
         (n (length v))
         (out (make-array n)))
    (dotimes (i n)
      (setf (aref out i)
            (coerce-value (aref v i) target-type :on-error on-error)))
    out))
