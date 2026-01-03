;;;; test/na-tests.lisp

(in-package #:cl-vctrs-lite/test)

;; M1 — NA tests

(test na-singleton-and-predicates
  (is (na-p *na*))
  (is (not (na-p nil)))
  (is (not (not-na-p *na*)))
  (is (not-na-p 42)))

(test na-printing
  (is (string= (princ-to-string *na*) "NA")))

(test na-equality
  ;; Propagate NA when either side is NA
  (is (eq (na=? *na* 1) *na*))
  (is (eq (na=? 1 *na*) *na*))
  (is (eq (na=? *na* *na*) *na*))
  ;; Otherwise use eql semantics
  (is (na=? 1 1))
  (is (not (na=? 1 2)))
  (is (na=? nil nil))
  (is (not (na=? t nil))))
