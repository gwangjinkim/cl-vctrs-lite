;;;; test/ops-tests.lisp

(in-package #:cl-vctrs-lite/test)

;; M7 — Vectorized operations

(test arith-vector-plus-scalar
  (is (equalp (v+ *v-int* 10) #(11 12 13)))
  (is (equalp (v* *v-int* *v-int*) #(1 4 9))))

(test arith-na-propagation
  (let ((r (v+ *v-mix* 1)))
    (is (= (aref r 0) 2))
    (is (eq (aref r 1) *na*))
    (is (= (aref r 2) 4))))

(test division-by-zero-error
  (let ((err (handler-case
                 (progn (v/ *v-int* 0) nil)
               (error (e) e))))
    (is err)
    (let ((msg (princ-to-string err)))
      (is (search "v/:" msg))
      (is (search "division by zero" msg)))))

(test incompatible-type-error
  (let ((err (handler-case
                 (progn (v+ #("a" "b") #(1 2)) nil)
               (error (e) e))))
    (is err)
    (let ((msg (princ-to-string err)))
      (is (search "v+:" msg))
      (is (search ":string" msg))
      (is (search ":int" msg)))))

(test comparisons-numeric-and-strings
  (is (equalp (v< #(1 2 3) 2) #(t nil nil)))
  (let ((r (v= (vector 1 *na* 3) (vector 1 2 3))))
    (is (eql (aref r 0) t))
    (is (eq (aref r 1) *na*))
    (is (eql (aref r 2) t)))
  (is (equalp (v= #("a" "b") #("a" "c")) #(t nil)))
  (is (equalp (v<= #(1 2) #(1 1)) #(t nil)))
  (is (equalp (v>= #("a" "b") #("a" "a")) #(t t))))
