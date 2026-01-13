# cl-vctrs-lite

## Why/Goals
- Provide a rock-solid “vectors + NA + recycling” core for future table libraries (tibble/dplyr/tidyr-like) in Common Lisp.
- Favor performance.
- Keep semantics small, predictable, and portable across CL implementations.
- Make vectorized operations safe: strict types, strict recycling, NA propagation, clear errors.
- Drive development with tests-first (FiveAM) to lock behavior.

A tiny, test-driven “vctrs-like” substrate for Common Lisp. It provides:
- A stable NA singleton with predicates and NA-aware equality
- A minimal type-tag system (`:na :bool :int :double :string :any`)
- A simple vector protocol (normalize, length, 0-based ref, map, subseq, col-type)
- Explicit coercion with predictable on-error behavior
- Strict recycling/broadcasting rules
- A few safe vectorized ops with NA propagation (`v+ v- v* v/ v= v< v<= v> v>=`)

This README reflects v0.1 as implemented through milestone M7.

## API Overview

### NA
| Symbol | Summary |
|---|---|
| `*na*` | Singleton missing value (prints as `NA`). |
| `na-p` | Predicate for NA. |
| `not-na-p` | Negated predicate. |
| `na=?` | NA-aware equality: returns `*na*` if either side is `*na*`, else EQL. |

### Types
| Symbol | Summary |
|---|---|
| `value-type` | Single value → type tag (`:na :bool :int :double :string :any`). |
| `common-type` | Minimal lattice for two tags (NA collapses to the other). |

### Protocol
| Symbol | Summary |
|---|---|
| `as-col` | Normalize list → `simple-vector`; arrays pass through; strings are scalars. |
| `col-length` | Length of a vector-like (0-based model). |
| `col-ref` | 0-based element access with bounds-checked errors. |
| `col->list` | Convert column to a list. |
| `col-map` | Map a function over a column; returns `simple-vector`. |
| `col-subseq` | Take elements at zero-based indices (list or vector); returns `simple-vector`. |
| `col-type` | Column type, ignoring NA, via `value-type`/`common-type`. |

### Coercion
| Symbol | Summary |
|---|---|
| `coerce-value` | Coerce a value to a target type; `:on-error` one of `:error :na :keep`. |
| `col-coerce` | Elementwise coercion for a column; returns `simple-vector`. |

### Recycling
| Symbol | Summary |
|---|---|
| `recycle-to` | Recycle scalar/len-1 vector to length `n`; strict otherwise (error). |
| `recycle2` | Recycle two columns to a common size or error for incompatible >1 lengths. |

| `recycle2` | Recycle two columns to a common size or error for incompatible >1 lengths. |
 
 ### Concatenation
 | Symbol | Summary |
 |---|---|
 | `vec-c` | Concatenate vectors with automatic type promotion (e.g., int + double → double). |
 
 ### Vectorized Ops
| Symbol | Summary |
|---|---|
| `v+ v- v* v/` | Arithmetic on numeric only; NA propagates; `v/` errors on divide-by-zero. |
| `v= v< v<= v> v>=` | Comparisons for numeric and strings; NA propagates. |

## Systems and Packages
- ASDF systems:
  - `:cl-vctrs-lite` (library)
  - `:cl-vctrs-lite/test` (test suite)
- Packages:
  - Library: `CL-VCTRS-LITE`
  - Tests: `CL-VCTRS-LITE/TEST`

## Installation
Local checkout via ASDF:

```lisp
(require :asdf)
;; from a REPL started in the repo root
(asdf:load-asd #P"cl-vctrs-lite.asd")
(asdf:load-system :cl-vctrs-lite)
```

Or just run tests (loads the system):

```bash
make test
```

Manual test run from REPL (same as the Makefile target):

```lisp
(ql:quickload :cl-vctrs-lite/test)
(asdf:test-system :cl-vctrs-lite/test)
```

## NA (Missing Value)

```lisp
CL-VCTRS-LITE:*NA*                 ;; prints as NA
(CL-VCTRS-LITE:NA-P *NA*)          ;; => T
(CL-VCTRS-LITE:NOT-NA-P 42)        ;; => T
(CL-VCTRS-LITE:NA=? *NA* 1)        ;; => NA (propagates)
(CL-VCTRS-LITE:NA=? 1 1)           ;; => T (EQL semantics for non-NA)
```

Printed representation is deterministic: `(princ-to-string *na*) => "NA"`.

## Type Tags

```lisp
(CL-VCTRS-LITE:VALUE-TYPE *NA*)    ;; => :NA
(CL-VCTRS-LITE:VALUE-TYPE 10)      ;; => :INT
(CL-VCTRS-LITE:VALUE-TYPE 1.5)     ;; => :DOUBLE
(CL-VCTRS-LITE:VALUE-TYPE "x")    ;; => :STRING
(CL-VCTRS-LITE:VALUE-TYPE t)       ;; => :BOOL
(CL-VCTRS-LITE:COMMON-TYPE :INT :DOUBLE)   ;; => :DOUBLE
```

## Column Protocol (v0.1)
- Vector-likes: Common Lisp arrays (including `simple-vector`) and lists. Strings are scalars.
- All indexing is 0-based.

```lisp
(defparameter *col* (CL-VCTRS-LITE:AS-COL '(10 20 30)))  ;; => #(10 20 30)
(CL-VCTRS-LITE:COL-LENGTH *col*)          ;; => 3
(CL-VCTRS-LITE:COL-REF *col* 0)           ;; => 10
(CL-VCTRS-LITE:COL-REF *col* 2)           ;; => 30
(CL-VCTRS-LITE:COL->LIST *col*)           ;; => (10 20 30)
(CL-VCTRS-LITE:COL-MAP #'1+ *col*)        ;; => #(11 21 31)
(CL-VCTRS-LITE:COL-SUBSEQ *col* '(2 0 1)) ;; => #(30 10 20)

;; Column type (ignores NA)
(CL-VCTRS-LITE:COL-TYPE (vector 1 *NA* 3)) ;; => :INT
```

When indices are out of range, errors include the function name and details:
"col-ref", the index, and the length.

## Coercion
Single values:

```lisp
(CL-VCTRS-LITE:COERCE-VALUE *NA* :double)        ;; => NA
(CL-VCTRS-LITE:COERCE-VALUE 1 :double)           ;; => 1.0d0
(CL-VCTRS-LITE:COERCE-VALUE 2.0 :int)            ;; => 2
(CL-VCTRS-LITE:COERCE-VALUE 2.3 :int :on-error :na)   ;; => NA
(CL-VCTRS-LITE:COERCE-VALUE "3.14" :double)      ;; error (v0.1 rejects strings)
(CL-VCTRS-LITE:COERCE-VALUE "3.14" :double :on-error :keep) ;; => "3.14"
```

Columns (lists or vectors; returns a simple-vector):

```lisp
(CL-VCTRS-LITE:COL-COERCE #(1 2 3) :double)      ;; => #(1.0d0 2.0d0 3.0d0)
(CL-VCTRS-LITE:COL-COERCE (vector 1 *NA* 3) :double) ;; => #(1.0d0 NA 3.0d0)
(CL-VCTRS-LITE:COL-COERCE #("a" "b") :double :on-error :na) ;; => #(NA NA)
```

Accepted targets in v0.1: `:string :double :int :bool :any`.

## Recycling (Broadcasting)
Strict rules: only scalars or length-1 vectors are broadcast; mismatched >1 lengths error.

```lisp
(CL-VCTRS-LITE:RECYCLE-TO 7 4)          ;; => #(7 7 7 7)
(CL-VCTRS-LITE:RECYCLE-TO #(5) 3)       ;; => #(5 5 5)
(multiple-value-bind (a2 b2 n)
    (CL-VCTRS-LITE:RECYCLE2 #(1 2 3) 10)
  (list a2 b2 n))                        ;; => (#(1 2 3) #(10 10 10) 3)
```

Errors mention the function and lengths, e.g.,
"recycle-to:" with the name, actual and expected lengths, or
"recycle2:" with “incompatible lengths”.

"recycle2:" with “incompatible lengths”.
 
 ## Concatenation
 
 ```lisp
 (CL-VCTRS-LITE:VEC-C #(1 2) #(3 4))            ;; => #(1 2 3 4)
 (CL-VCTRS-LITE:VEC-C #(1) 2.0 #(3))            ;; => #(1.0 2.0 3.0) (:INT + :DOUBLE -> :DOUBLE)
 ```
 
 ## Vectorized Operations
All ops accept scalars or vectors/lists, recycle via `recycle2`, and propagate NA.
Arithmetic supports numeric types only; comparisons support numeric and strings.

```lisp
(CL-VCTRS-LITE:V+ #(1 2 3) 10)          ;; => #(11 12 13)
(CL-VCTRS-LITE:V* #(1 2 3) #(1 2 3))    ;; => #(1 4 9)
(CL-VCTRS-LITE:V+ (vector 1 *NA* 3) 1)  ;; => #(2 NA 4)

;; Comparisons (T/NIL/NA)
(CL-VCTRS-LITE:V< #(1 2 3) 2)           ;; => #(T NIL NIL)
(CL-VCTRS-LITE:V= #("a" "b") #("a" "c")) ;; => #(T NIL)

;; Division by zero raises an error
(handler-case (CL-VCTRS-LITE:V/ #(1 2) 0)
  (error (e) (format nil "~a" e)))     ;; contains "v/: division by zero"

;; Incompatible types raise errors mentioning the op and type tags
(handler-case (CL-VCTRS-LITE:V+ #("a" "b") #(1 2))
  (error (e) (format nil "~a" e)))     ;; contains "v+:" and ":string" ":int"
```

## Development
- Run all tests: `make test` (Roswell + FiveAM)
- Test system: `:cl-vctrs-lite/test`
- Error messages are prefixed with the function name (e.g., "v+:", "recycle-to:").

## Scope and Caveats (v0.1)
- Minimal, explicit type lattice; `:any` for mixed/unknown.
- Strings are scalar values (not vectors in this protocol).
- Recycling is strict: only scalars or length-1 vectors broadcast.
- Coercion deliberately rejects string→double in v0.1 (use `:on-error` to soften).

## License
MIT (see system definition).
