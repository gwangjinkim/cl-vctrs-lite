;;;; test/logic-tests.lisp

(in-package #:cl-vctrs-lite/test)

(fiveam:in-suite :suite)

;; Missingness and Kleene three-valued logic

(test v-na-p-is-vectorized
  (is (equalp (v-na-p *v-mix*) #(nil t nil)))
  (is (equalp (v-not-na-p *v-mix*) #(t nil t))))

(test v-not-keeps-na
  (let ((r (v-not (vector t nil *na*))))
    (is (eq (aref r 0) nil))
    (is (eq (aref r 1) t))
    (is (eq (aref r 2) *na*))))

(test v-and-kleene
  (let ((r (v-and (vector t t nil *na* *na*)
                  (vector t nil *na* t nil))))
    (is (eq (aref r 0) t))
    (is (eq (aref r 1) nil))
    (is (eq (aref r 2) nil))      ; FALSE & NA = FALSE
    (is (eq (aref r 3) *na*))     ; NA & TRUE = NA
    (is (eq (aref r 4) nil))))

(test v-or-kleene
  (let ((r (v-or (vector nil nil t *na* *na*)
                 (vector nil t *na* nil t))))
    (is (eq (aref r 0) nil))
    (is (eq (aref r 1) t))
    (is (eq (aref r 2) t))        ; TRUE | NA = TRUE
    (is (eq (aref r 3) *na*))     ; NA | FALSE = NA
    (is (eq (aref r 4) t))))

(test v-and-recycles-scalar
  (is (equalp (v-and (vector t nil t) t) #(t nil t))))

(test v-not-equal
  (let ((r (v!= *v-mix* 1)))
    (is (eq (aref r 0) nil))
    (is (eq (aref r 1) *na*))
    (is (eq (aref r 2) t))))

(test v-not-rejects-non-logical
  (signals error (v-not #(1 2))))
