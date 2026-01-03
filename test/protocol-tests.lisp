;;;; test/protocol-tests.lisp

(in-package #:cl-vctrs-lite/test)

;; M3 — Protocol basics: as-col, col-length, col-ref, col->list

(test as-col-normalizes-list
  (let* ((lst '(1 2 3))
         (col (as-col lst)))
    (is (typep col 'simple-vector))
    (is (equalp col #(1 2 3)))))

(test col-length-works
  (is (= (col-length #(1 2 3 4)) 4))
  (is (= (col-length '(a b c)) 3)))

(test col-ref-0-based
  (let ((col *v-int*))
    (is (= (col-ref col 0) 1))
    (is (= (col-ref col 2) 3)))
  (let ((col (as-col '(10 20 30))))
    (is (= (col-ref col 1) 20))))

(test col-ref-out-of-range-error-message
  (let* ((col *v-int*)
         (n (col-length col))
         (bad-index 5)
         (err (handler-case
                   (progn (col-ref col bad-index)
                          nil)
                 (error (e) e))))
    (is err)
    (let ((msg (princ-to-string err)))
      (is (search "col-ref" msg))
      (is (search (write-to-string bad-index) msg))
      (is (search (write-to-string n) msg)))))

(test col->list-roundtrip
  (is (equal (col->list #(1 2 3)) '(1 2 3)))
  (is (equal (col->list '(1 2 3)) '(1 2 3))))

;; M4 — col-map / col-subseq / col-type

(test col-map-preserves-length
  (let* ((col *v-int*)
         (mapped (col-map #'identity col)))
    (is (= (col-length mapped) (col-length col))))
  (let* ((lst '(10 20 30))
         (mapped (col-map (lambda (x) (+ x 1)) lst)))
    (is (= (col-length mapped) 3))))

(test col-subseq-by-indices
  (let* ((col *v-int*)
         (indices '(2 0 1))
         (res (col-subseq col indices)))
    (is (equalp res #(3 1 2)))
    (is (equal (col->list res) '(3 1 2))))
  (let* ((col *v-int*)
         (indices #(1 1 2)))
    (is (equalp (col-subseq col indices) #(2 2 3)))))

(test col-type-ignores-na
  (is (eql (col-type *v-mix*) :int))
  (is (eql (col-type (vector 1.0 *na* 2)) :double))
  (is (eql (col-type (vector "a" *na* "b")) :string)))
