# blatent 0.1.3 — CRAN submission comments

## Reason for this submission

This is a maintenance release in response to the Rcpp/CRAN maintainer email
asking packages that `LinkingTo: Rcpp` to regenerate `RcppExports.cpp` with
Rcpp >= 1.1.1. The regenerated file now emits the guarded `(Rf_error)(...)`
form, which is safe against the macro expansion that the upcoming compiler
warning (planned for July) will flag.

## What changed

* `src/RcppExports.cpp` and related auto-generated files regenerated with
  Rcpp 1.1.1 via `Rcpp::compileAttributes()`.
* `DESCRIPTION`: version bumped to 0.1.3; `Imports: Rcpp (>= 1.1.1)`.
* `NEWS.md`: entry for 0.1.3.

No user-visible behavior changes.

## Test environments

* Local: macOS (aarch64-apple-darwin20), R 4.5.2, Rcpp 1.1.1.
* `R CMD check --as-cran`: 0 errors, 0 warnings (notes, if any, noted below).

## R CMD check results

(To be filled in with the final check output before submission.)

## Downstream dependencies

None known.
