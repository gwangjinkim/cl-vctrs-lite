# AGENTS.md — cl-vctrs-lite (includes cl-na)
Purpose: provide a tiny, rock-solid “vectors + missing values + recycling” foundation for a future
`tibble/dplyr/tidyr` stack in Common Lisp.

This repo intentionally bundles:
- **cl-na** (missing value singleton + semantics)
- **cl-vctrs-lite** (vector protocol, type tags, coercion, recycling, and a few safe ops)

Agents: treat this file as the **source of truth**. Implement **milestone by milestone** with tests-first.

---

## 0) Non-negotiable workflow rules for Codex/agents

For each milestone (M0, M1, …):
1. Create or update **tests first** (FiveAM).
2. Implement the minimal code to pass tests.
3. Run `(asdf:test-system :cl-vctors-lite/test)` and fix failures.
4. Summarize changes + list touched files.
5. STOP. Do not start next milestone unless asked.

No giant refactors. No speculative features.

---

## 1) Repo structure (required)
```
cl-vctrs-lite/
cl-vctrs-lite.asd
README.md
src/
package.lisp
na.lisp
types.lisp
protocol.lisp
coerce.lisp
recycle.lisp
ops.lisp
util.lisp
test/
package.lisp
suite.lisp
na-tests.lisp
protocol-tests.lisp
coerce-tests.lisp
recycle-tests.lisp
ops-tests.lisp
```

### ASDF systems
- `:cl-vctrs-lite`
- `:cl-vctors-lite/test` depends on `:cl-vctrs-lite` and `:fiveam`

### Packages
- Main: `CL-VCTRS-LITE`
- Tests: `CL-VCTRS-LITE/TEST`

---

## 2) Design goals

### What we are building (v0.1)
A minimal “vctrs-like” substrate:

- **NA**: a singleton missing value + helper predicates
- **Vector protocol**: length/ref/subseq/map
- **Type tags**: `:int :double :string :bool :any` (and `:na` for the singleton)
- **Coercion**: minimal and explicit
- **Recycling rules**: scalar broadcast and strict size checks
- **A few safe vectorized ops**: `v+ v- v* v/ v= v< v>` with NA propagation

### What we are NOT building (v0.1)
- full R vctrs (no S3, no full type lattice)
- fancy UTF-8 collation, locale, timezones
- SIMD performance tricks
- lazy vectors

Correctness > features.

---

## 3) Missing values (cl-na inside this repo)

### 3.1 NA object
Define a unique singleton `*na*` in `na.lisp`.

Requirements:
- `*na*` must be unique and printable in a stable way.
- `(na-p *na*) => T`
- `(na-p anything-else) => NIL`

Recommended implementation:
- a DEFSTRUCT with no slots and one instance, OR
- a GENSYM stored in `*na*` (but then printing control is harder).
Prefer struct.

### 3.2 Public API (NA)
Export from `CL-VCTRS-LITE`:
- `*na*`
- `na-p`
- `not-na-p`
- `na=?` (NA-aware equality predicate; see below)
- `na-or` helper (optional v0.1, but helpful)

Semantics:
- `na=?`:
  - if either argument is `*na*`, return `*na*` (propagate)
  - else return `(eql a b)` (or `equal` for strings? decide; see section 4 types)
- `not-na-p` is simply `(not (na-p x))`

### 3.3 Printing NA
Printing `*na*` must be deterministic and short.
Example preferred printed representation: `#<NA>` or `NA`.

Test requirement:
- `(princ-to-string *na*)` equals `"NA"` (choose one and lock it).

---

## 4) Type system (lite)

### 4.1 Type tags
Define keywords:
- `:na` for `*na*`
- `:bool` for `t`/`nil`
- `:int` for integers
- `:double` for floats
- `:string` for strings
- `:any` for mixed/unknown

Export:
- `value-type` (single value -> tag)
- `common-type` (two tags -> tag)
- `col-type` (vector -> tag) (declared in protocol but implemented using these rules)

Rules:
- `value-type(*na*) => :na`
- `value-type(t or nil) => :bool`
- integers => `:int`
- floats => `:double`
- strings => `:string`
- otherwise => `:any`

### 4.2 common-type (minimal lattice)
Implement:
- `:na` with anything => other type (NA can live in any column)
- `:int + :double => :double`
- `:int + :int => :int`
- `:double + :double => :double`
- `:string + :string => :string`
- `:bool + :bool => :bool`
- mixed otherwise => :any

Important: This is a deliberate simplification.

---

## 5) Vector protocol

### 5.1 Supported column representations (v0.1)
We support:
- CL arrays (including simple-vectors)
- lists (as input; but we may normalize to vectors internally if desired)
- strings are NOT vectors here (treat as scalar string values)

We define a “vector-like” as:
- an array OR a list (v0.1)
Later we can add custom vector classes.

### 5.2 Public protocol functions
Export:

- `col-length (col) -> integer`
- `col-ref (col i) -> value` (0-based indexing!)
- `col-subseq (col indices) -> col` where indices is a vector/list of integers
- `col-map (fn col) -> col` returns same representation as input if feasible (v0.1 can always return a simple-vector)
- `col-type (col) -> type-tag` (uses value-type/common-type, ignoring NA)
- `as-col (x) -> col` normalize list -> simple-vector, vector -> itself
- `col->list (col) -> list` (for convenience/testing)

Rules:
- `col-ref` uses 0-based indexing (like arrays in CL). This must be consistent everywhere.
- `col-subseq` preserves order of indices.

### 5.3 Error messages
When index out of range:
- signal `simple-error` with message containing:
  - `"col-ref"`
  - the index
  - the length

Tests must validate error contains those substrings.

---

## 6) Coercion

Export:
- `col-coerce (col target-type &key (on-error :error)) -> new-col`
- `coerce-value (x target-type &key (on-error :error)) -> value`

Rules:
- `*na*` coerces to `*na*` for any target type.
- To `:string`: use `princ-to-string` for non-string values.
- To `:double`: integers convert to float; strings only if parseable (v0.1 can reject strings).
- To `:int`: floats only if integer-valued? (v0.1: reject floats unless exact integer, or always reject; pick one and test it)
- To `:bool`: only accept `t`, `nil`, or strings `"true"/"false"`? (v0.1: only accept boolean values)

On error:
- if `on-error :error` => signal an error
- if `on-error :na` => return `*na*` for that element
- if `on-error :keep` => keep original value

Tests must cover these three modes.

---

## 7) Recycling rules (core of vctrs-lite)

Export:
- `recycle-to (col n &key (name "value")) -> col`
- `recycle2 (a b &key (name-a "a") (name-b "b")) -> (values a2 b2 n)`
- `broadcast-scalar (x n) -> col` (helper; may be internal)

Definitions:
- Scalar means a non-vector-like value OR a vector-like of length 1.
- `recycle-to` rules:
  - if length(col) == n => return as-col(col)
  - if length(col) == 1 => broadcast to length n
  - else error with message:
    - includes `"recycle"`
    - includes `name`, and expected vs actual lengths

- `recycle2`:
  - Let `na = col-length(a)`, `nb = col-length(b)` after treating scalars as length 1 vectors.
  - `n = max(na, nb)`.
  - Each is recycled to `n` using `recycle-to`.
  - If both lengths are >1 and not equal => error (“incompatible lengths”)

We intentionally implement *strict* recycling (closer to vctrs than base R):
- No silent partial recycling except length 1.

---

## 8) Vectorized operations (minimal)

Export:
- Arithmetic: `v+ v- v* v/`
- Comparisons: `v= v< v> v<= v>=`
- Logic: `vand vor vnot` (optional v0.1)

All ops must:
- Accept scalars or vectors/lists.
- Use `recycle2` internally.
- Propagate NA:
  - If either operand at position i is `*na*`, result is `*na*` (for arithmetic and comparisons).
- Type behavior:
  - arithmetic supports numeric (`:int`, `:double`) only
  - comparisons:
    - numeric compares numeric
    - string compares string using `string<` etc.
    - bool compares bool (optional)
  - for unsupported combos, error with message containing function name + types involved.

Division:
- if divisor is 0 => signal error OR return `*na*`? Choose and document.
Recommended v0.1: **signal error** (“division by zero”) because silent NA is dangerous. Add tests.

Return representation:
- Always return a simple-vector in v0.1.

---

## 9) Test requirements (FiveAM)

### 9.1 Test style
- Each public function must have at least one positive test and one negative/error test where applicable.
- Use small vectors for expected results.

### 9.2 Required fixtures
In `test/suite.lisp`, define:
- `*v-int*  #(1 2 3)`
- `*v-dbl*  #(1.0 2.0 3.0)`
- `*v-mix*  #(1 *na* 3)`
- `*v-str*  #("a" "b" "c")`

---

## 10) Milestones

### M0 — Scaffold
**Deliverables**
- ASDF files
- packages
- empty test suite passes
**Tests**
- trivial `(is t)` smoke test

### M1 — NA
**Implement**
- `*na*` + printing
- `na-p`, `not-na-p`, `na=?`
**Tests**
- na singleton, predicates
- printing golden test

### M2 — Type tags
**Implement**
- `value-type`, `common-type`
**Tests**
- value-type for int/double/string/bool/any/na
- common-type matrix tests

### M3 — Protocol basics
**Implement**
- `as-col`, `col-length`, `col-ref`, `col->list`
**Tests**
- list input normalized
- 0-based indexing correctness
- out-of-range error contains required substrings

### M4 — col-map / col-subseq / col-type
**Implement**
- `col-map`, `col-subseq`, `col-type`
**Tests**
- map preserves length
- subseq by indices vector/list
- col-type ignores NA, uses common-type

### M5 — Coercion
**Implement**
- `coerce-value`, `col-coerce`
- on-error behaviors
**Tests**
- int->double ok
- na stays na
- invalid string->double errors
- on-error :na produces NA

### M6 — Recycling
**Implement**
- `recycle-to`, `recycle2`
**Tests**
- scalar broadcast
- len1 vector broadcast
- incompatible lengths error message check

### M7 — Ops
**Implement**
- `v+ v- v* v/`
- `v= v< v>`
**Tests**
- vector + scalar
- vector + vector
- NA propagation
- division by zero error
- incompatible type error

Stop after M7 unless asked.

---

## 11) Public API list (must match exports)

Export exactly these (v0.1):

NA:
- `*na*` `na-p` `not-na-p` `na=?`

Types:
- `value-type` `common-type`

Protocol:
- `as-col` `col-length` `col-ref` `col-subseq` `col-map` `col-type` `col-coerce` `col->list`

Recycling:
- `recycle-to` `recycle2`

Ops:
- `v+` `v-` `v*` `v/` `v=` `v<` `v>` `v<=` `v>=`

If you add anything else, update this section and add tests.

---

## 12) Coding conventions

- Use `simple-vector` for outputs unless there is a strong reason not to.
- Keep functions small; prefer helpers in `util.lisp`.
- Error messages: start with function name prefix like `"v+:"`, `"recycle-to:"`, etc.
- Avoid implementation-specific features; keep portable CL.

---

## 13) README minimum
Must include a minimal usage section that corresponds to tests:
- NA printing
- recycle examples
- v+ examples

---

## 14) How to run (tests + REPL)

Follow the canonical commands in `ROSWELL.md`.

Agents must run tests via:

```bash
make test
```

If `make test` is not available (bootstrap only), follow the fallback instructions in ROSWELL.md,
then create the required scripts so make test works thereafter.


---
End of AGENTS.md
