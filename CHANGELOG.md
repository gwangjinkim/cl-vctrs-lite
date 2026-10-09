# Changelog

All notable changes to this project are documented here. Format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/). See AGENTS.md
("Change Discipline") for how entries and commits are written.

## [Unreleased]

### Tests
- **The test suite now actually runs.** `asdf:test-system :cl-vctrs-lite`
  ran nothing because the main system had no `:in-order-to` test-op, and
  even `cl-vctrs-lite/test` only ran the 4 tests in `concat-tests.lisp`:
  every other test file lacked `(fiveam:in-suite :suite)`, so its tests were
  registered outside the suite. Both fixed; the suite went from 13 checks
  to 116 (topic: test-suite repair).
- Replaced bare `(is err)` assertions with `(is-true err)`; current FiveAM
  rejects a non-list argument to `IS` at macroexpansion time, which turned
  7 tests into errors.
