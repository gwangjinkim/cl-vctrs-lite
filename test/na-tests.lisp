;;;; test/na-tests.lisp

(in-package #:cl-vctrs-lite/test)

(test na-singleton-and-printing
  (is (na-p *na*))
  (is (not (na-p 123)))
  (is (string= "NA" (princ-to-string *na*))))

(test na-propagating-equality
  (is (eql *na* (na=? *na* 1)))
  (is (eql *na* (na=? 1 *na*)))
  (is (eql t (na=? 1 1)))
  (is (eql nil (na=? 1 2))))
