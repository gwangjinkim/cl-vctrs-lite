# cl-vctrs-lite

A tiny, test-driven “vctrs-like” foundation for a future tibble/dplyr/tidyr stack in Common Lisp.

## Run tests

```lisp
(ql:quickload :cl-vctrs-lite-tests)
(asdf:test-system :cl-vctrs-lite-tests)
```

---

## NA

```lisp
CL-VCTRS-LITE:*NA*  ;; prints as NA
(CL-VCTRS-LITE:NA-P CL-VCTRS-LITE:*NA*)  ;; => T
```

---

## Quick start (what to run in your REPL)

```lisp
(ql:quickload :cl-vctrs-lite-tests)
(asdf:test-system :cl-vctrs-lite-tests)
