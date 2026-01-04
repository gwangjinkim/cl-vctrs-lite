;;;; test/coerce-tests.lisp

(in-package #:cl-vctrs-lite/test)

;; M5 — Coercion tests

(test coerce-value-int-to-double
  (let ((r (coerce-value 1 :double)))
    (is (floatp r))
    (is (= r 1.0))))

(test coerce-value-na-stays-na
  (is (eq (coerce-value *na* :int) *na*))
  (is (eq (coerce-value *na* :double) *na*))
  (is (eq (coerce-value *na* :string) *na*)))

(test coerce-value-string-to-double-error
  (let ((err (handler-case
                 (progn (coerce-value "3.14" :double) nil)
               (error (e) e))))
    (is err)))

(test coerce-value-on-error-na
  (is (eq (coerce-value "3.14" :double :on-error :na) *na*)))

(test coerce-value-on-error-keep
  (is (string= (coerce-value "3.14" :double :on-error :keep) "3.14")))

(test col-coerce-vector
  (is (equalp (col-coerce #(1 2 3) :double)
              #(1.0 2.0 3.0)))
  (is (equalp (col-coerce *v-mix* :double)
              (vector 1.0 *na* 3.0))))

(test col-coerce-error-and-modes
  (let ((err (handler-case
                 (progn (col-coerce #("a" "b") :double) nil)
               (error (e) e))))
    (is err))
  (is (equalp (col-coerce #("a" "b") :double :on-error :na)
              (vector *na* *na*)))
  (is (equalp (col-coerce #("a" "b") :double :on-error :keep)
              #("a" "b"))))
