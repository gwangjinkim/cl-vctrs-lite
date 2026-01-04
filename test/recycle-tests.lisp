;;;; test/recycle-tests.lisp

(in-package #:cl-vctrs-lite/test)

;; M6 — Recycling tests

(test recycle-to-broadcast-scalar
  (is (equalp (recycle-to 7 4) #(7 7 7 7)))
  (is (equalp (recycle-to "x" 2) #("x" "x"))))

(test recycle-to-len1-vector
  (is (equalp (recycle-to #(5) 3) #(5 5 5)))
  (is (equalp (recycle-to '(5) 3) #(5 5 5))))

(test recycle-to-same-length
  (is (equalp (recycle-to #(1 2 3) 3) #(1 2 3))))

(test recycle-to-error-message
  (let* ((bad #(1 2))
         (err (handler-case
                   (progn (recycle-to bad 3 :name "x") nil)
                 (error (e) e))))
    (is err)
    (let ((msg (princ-to-string err)))
      (is (search "recycle" msg))
      (is (search "x" msg))
      (is (search "2" msg))
      (is (search "3" msg)))))

(test recycle2-broadcast-and-length
  (multiple-value-bind (a2 b2 n)
      (recycle2 #(1 2 3) 10)
    (is (= n 3))
    (is (equalp a2 #(1 2 3)))
    (is (equalp b2 #(10 10 10)))))

(test recycle2-incompatible-lengths-error
  (let ((err (handler-case
                 (progn (recycle2 #(1 2) #(1 2 3)) nil)
               (error (e) e))))
    (is err)
    (let ((msg (princ-to-string err)))
      (is (search "incompatible" msg))
      (is (search "length" msg))
      (is (search "2" msg))
      (is (search "3" msg)))) )
