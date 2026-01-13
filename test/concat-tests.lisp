;;;; test/concat-tests.lisp

(in-package #:cl-vctrs-lite/test)

(fiveam:in-suite :suite)

;; Fixtures are available from :suite if needed, but we can verify usually with literals.

(test vec-c-basics
  (is (equalp #(1 2 3) (vec-c #(1) #(2 3))))
  (is (equalp #(1 2 3 4) (vec-c 1 #(2 3) 4))) ;; Scalars + Vectors
  (is (equalp #() (vec-c))))

(test vec-c-promotion
  ;; Int + Double -> Double
  (is (equalp #(1.0 2.5) (vec-c 1 2.5)))
  (is (eq :double (col-type (vec-c 1 2.5))))
  
  ;; Int + String -> String (if supported by common-type rules, usually :any or :string)
  ;; M2 says :string + :string -> :string, others -> :any usually?
  ;; Let's check common-type rules in AGENTS.md:
  ;; :int + :double => :double
  ;; mixed otherwise => :any
  ;; So int + string -> :any
  (is (eq :any (col-type (vec-c 1 "a")))))

(test vec-c-na
  (let ((res (vec-c 1 *na* 2)))
    (is (= 3 (length res)))
    (is (= 1 (aref res 0)))
    (is (na-p (aref res 1)))
    (is (= 2 (aref res 2)))
    (is (eq :int (col-type res)))))

(test vec-c-empty-inputs
  (is (equalp #(1.0) (vec-c #() 1.0 #())))
  (is (equalp #() (vec-c #() #()))))
