;;;; src/types.lisp

(in-package #:cl-vctrs-lite)

(defun value-type (x)
  "Return a keyword type tag for value X."
  (cond
    ;; NA singleton
    ((na-p x) :na)
    ;; booleans
    ((or (eq x t) (null x)) :bool)
    ;; integers
    ((integerp x) :int)
    ;; floats
    ((floatp x) :double)
    ;; strings
    ((stringp x) :string)
    ;; otherwise
    (t :any)))

(defun common-type (a b)
  "Return the minimal common type for tags A and B."
  (cond
    ;; NA with anything => the other type
    ((eq a :na) b)
    ((eq b :na) a)
    ;; numeric lattice
    ((and (eq a :int) (eq b :int)) :int)
    ((and (eq a :double) (eq b :double)) :double)
    ((or (and (eq a :int) (eq b :double))
         (and (eq a :double) (eq b :int))) :double)
    ;; homogeneous non-numeric
    ((and (eq a :string) (eq b :string)) :string)
    ((and (eq a :bool) (eq b :bool)) :bool)
    ;; otherwise mixed => :any
    (t :any)))
