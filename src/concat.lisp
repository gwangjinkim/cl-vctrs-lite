;;;; src/concat.lisp

(in-package #:cl-vctrs-lite)

(defun vec-c (&rest cols)
  "Concatenate COLS into a single vector with a common type.
1. Computes the common type of all inputs.
2. Coerces each input to that common type.
3. Concatenates them into a fresh simple-vector."
  (let* ((cols (mapcar (lambda (x)
                         (if (%vector-like-p x)
                             (as-col x)
                             (vector x)))
                       cols))
         (ntotal (reduce #'+ cols :key #'col-length)))
    
    (when (zerop ntotal)
      (return-from vec-c #()))

    ;; Determine common type for all columns
    (let ((ctype (reduce #'common-type cols 
                         :key #'col-type 
                         :initial-value :int))) ;; Start with :int as identity for common-type ascension? 
      ;; Actually, common-type isn't fully associative/commutative starting from NIL/void.
      ;; Better approach: take first col's type, then accumulate.
      ;; However, if we have no columns, we returned early.
      ;; If we have columns but they are all empty?
      ;; vec-c() -> #().
      ;; vec-c(int(), double()) -> empty double().
      
      ;; Let's refine the type reduction.
      ;; We need to reduce the types of the provided columns.
      (setf ctype 
            (if cols
                (reduce #'common-type (cdr cols)
                        :key #'col-type
                        :initial-value (col-type (car cols)))
                :any)) ;; Should be unreachable due to ntotal check if cols is empty, but just in case.

      ;; Coerce all to that type
      (let ((coerced-cols (mapcar (lambda (c) (col-coerce c ctype)) cols))
            (result (make-array ntotal :initial-element *na*))) ;; Simple-vector
        
        (loop with offset = 0
              for c in coerced-cols
              do (loop for i from 0 below (col-length c)
                       do (setf (aref result (+ offset i)) 
                                (col-ref c i)))
                 (incf offset (col-length c)))
        result))))
