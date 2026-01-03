;;;; test/types-tests.lisp — M2 tests for value-type and common-type

(in-package #:cl-vctrs-lite/test)

(test value-type-basic-tags
  ;; NA
  (is (eql (value-type *na*) :na))
  ;; booleans
  (is (eql (value-type t) :bool))
  (is (eql (value-type nil) :bool))
  ;; integers
  (is (eql (value-type 42) :int))
  ;; doubles
  (is (eql (value-type 3.14) :double))
  ;; strings
  (is (eql (value-type "hi") :string))
  ;; anything else -> :any
  (is (eql (value-type #\A) :any)))

(test common-type-minimal-lattice
  ;; NA with anything => other
  (is (eql (common-type :na :int) :int))
  (is (eql (common-type :double :na) :double))
  ;; numeric combos
  (is (eql (common-type :int :int) :int))
  (is (eql (common-type :double :double) :double))
  (is (eql (common-type :int :double) :double))
  (is (eql (common-type :double :int) :double))
  ;; homogeneous non-numeric
  (is (eql (common-type :string :string) :string))
  (is (eql (common-type :bool :bool) :bool))
  ;; mixed otherwise => :any
  (is (eql (common-type :string :int) :any))
  (is (eql (common-type :bool :double) :any))
  (is (eql (common-type :string :double) :any)))

