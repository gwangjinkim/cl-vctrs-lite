;;;; src/ops.lisp

(in-package #:cl-vctrs-lite)

(defun %both-numeric-p (tx ty)
  (and (member tx '(:int :double))
       (member ty '(:int :double))))

(defun %fmt-type (tag)
  (string-downcase (format nil "~s" tag)))

(defun %arith-op (name a b fn)
  (multiple-value-bind (va vb n) (recycle2 a b)
    (let ((out (make-array n)))
      (dotimes (i n)
        (let ((x (aref va i))
              (y (aref vb i)))
          (if (or (na-p x) (na-p y))
              (setf (aref out i) *na*)
              (let ((tx (value-type x))
                    (ty (value-type y)))
                (unless (%both-numeric-p tx ty)
                  (%simple-error "~a unsupported operand types ~a and ~a"
                                 name (%fmt-type tx) (%fmt-type ty)))
                (when (and (string= name "v/:") (zerop y))
                  (%simple-error "v/: division by zero"))
                (setf (aref out i) (funcall fn x y))))))
      out)))

(defun v+ (a b) (%arith-op "v+:" a b #'+))
(defun v- (a b) (%arith-op "v-:" a b #'-))
(defun v* (a b) (%arith-op "v*:" a b #'*))

(defun v/ (a b)
  (%arith-op "v/:" a b #'/))

(defun v= (a b)
  (multiple-value-bind (va vb n) (recycle2 a b)
    (let ((out (make-array n)))
      (dotimes (i n)
        (let ((x (aref va i))
              (y (aref vb i)))
          (if (or (na-p x) (na-p y))
              (setf (aref out i) *na*)
              (let ((tx (value-type x))
                    (ty (value-type y)))
                (cond
                  ((%both-numeric-p tx ty)
                   (setf (aref out i) (if (= x y) t nil)))
                  ((and (eq tx :string) (eq ty :string))
                   (setf (aref out i) (if (string= x y) t nil)))
                  ((and (eq tx :bool) (eq ty :bool))
                   (setf (aref out i) (if (eql x y) t nil)))
                  (t
                   (%simple-error "v=: unsupported operand types ~a and ~a"
                                  (%fmt-type tx) (%fmt-type ty))))))))
      out)))

(defun %order-op (name a b num-fn str-fn)
  (multiple-value-bind (va vb n) (recycle2 a b)
    (let ((out (make-array n)))
      (dotimes (i n)
        (let ((x (aref va i))
              (y (aref vb i)))
          (if (or (na-p x) (na-p y))
              (setf (aref out i) *na*)
              (let ((tx (value-type x))
                    (ty (value-type y)))
                (cond
                  ((%both-numeric-p tx ty)
                   (setf (aref out i) (if (funcall num-fn x y) t nil)))
                  ((and (eq tx :string) (eq ty :string))
                   (setf (aref out i) (if (funcall str-fn x y) t nil)))
                  (t
                   (%simple-error "~a unsupported operand types ~a and ~a"
                                  name (%fmt-type tx) (%fmt-type ty))))))))
      out)))

(defun v< (a b) (%order-op "v<:" a b #'< #'string<))
(defun v> (a b) (%order-op "v>:" a b #'> #'string>))
(defun v<= (a b) (%order-op "v<=:" a b #'<= #'string<=))
(defun v>= (a b) (%order-op "v>=:" a b #'>= #'string>=))
