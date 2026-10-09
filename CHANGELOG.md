# Changelog

All notable changes to this project are documented here. Format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/). See AGENTS.md
("Change Discipline") for how entries and commits are written.

## [Unreleased]

### Added
- `v-na-p`, `v-not-na-p`: vectorised missingness tests returning T/NIL
  vectors (never NA). Lets `filter` and `mutate` test for missing values
  column-wise (topic: NA-aware logic).
- `v-not`, `v-and`, `v-or`: vectorised, recycling three-valued (Kleene)
  logic. `NA & FALSE = FALSE`, `NA | TRUE = TRUE`, otherwise NA propagates,
  as in R. Non-logical inputs signal an error.
- `v!=`: vectorised inequality with NA propagation.
- These were previously improvised inside cl-dplyr (where `&` and `|`
  never worked); cl-dplyr now uses these.

### Changed
- Stopped tracking `build/` (38 files, 960K): the ASDF/Roswell compile
  cache that `make test` writes via `XDG_CACHE_HOME=$(PWD)/build`. It held
  compiled fasls, including Quicklisp internals and absolute local paths.
  `build/` is now in `.gitignore`; local files are untouched. Old copies
  remain in history (topic: repo hygiene).

### Tests
- **The test suite now actually runs.** `asdf:test-system :cl-vctrs-lite`
  ran nothing because the main system had no `:in-order-to` test-op, and
  even `cl-vctrs-lite/test` only ran the 4 tests in `concat-tests.lisp`:
  every other test file lacked `(fiveam:in-suite :suite)`, so its tests were
  registered outside the suite. Both fixed; the suite went from 13 checks
  to 114 (134 with the new logic tests) (topic: test-suite repair).
- Replaced bare `(is err)` assertions with `(is-true err)`; current FiveAM
  rejects a non-list argument to `IS` at macroexpansion time, which turned
  7 tests into errors.
