;;;; cl-vctrs-lite.asd

(asdf:defsystem #:cl-vctrs-lite
  :description "Lite vctrs-like substrate for Common Lisp (includes NA + vector protocol + recycling)."
  :author "You"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "util")
     (:file "na")
     (:file "types")
     (:file "protocol")
     (:file "coerce")
     (:file "recycle")
     (:file "ops")))))

(asdf:defsystem #:cl-vctrs-lite-tests
  :depends-on (#:cl-vctrs-lite #:fiveam)
  :serial t
  :components
  ((:module "test"
    :serial t
    :components
    ((:file "package")
     (:file "suite")
     (:file "na-tests")
     (:file "protocol-tests")
     (:file "coerce-tests")
     (:file "recycle-tests")
     (:file "ops-tests"))))
  :perform (asdf:test-op (op sys)
             (declare (ignore op sys))
             (uiop:symbol-call :fiveam :run! :suite)))
