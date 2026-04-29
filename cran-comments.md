# CRAN Submission — ViewR 0.2.0

## Test environments

| Environment | R version | Result |
|---|---|---|
| Local macOS (Apple Silicon) | R 4.4.x | 0 errors, 0 warnings, 0 notes |
| GitHub Actions — ubuntu-latest | R release | 0 errors, 0 warnings, 0 notes |
| GitHub Actions — ubuntu-latest | R devel | 0 errors, 0 warnings, 0 notes |
| GitHub Actions — windows-latest | R release | 0 errors, 0 warnings, 0 notes |
| GitHub Actions — macos-latest  | R release | 0 errors, 0 warnings, 0 notes |
| win-builder (devel)            | R devel   | 0 errors, 0 warnings, 1 note* |
| macOS builder (arm64)          | R release | 0 errors, 0 warnings, 0 notes |

\* The single NOTE on win-builder is "New submission" — expected for a
first CRAN submission.

## R CMD check results

```
0 errors | 0 warnings | 1 note
```

**NOTE (win-builder only):**
```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Mahesh Divakaran <imaheshdivakaran@gmail.com>'
New submission
```
This is expected for a new submission.

## Package size

Installed package size: < 1 MB.
No compiled code; pure R package.

## Downstream dependencies

None — this is a new package with no reverse dependencies.

## Notes on examples

All `ViewR()` examples are wrapped in `\dontrun{}` because they launch
an interactive Shiny gadget that requires a browser or RStudio session.
This is the recommended approach for interactive applications per CRAN
policy and the `shiny` package itself.

`install_viewr_deps()` examples are also in `\dontrun{}` because they
install packages, which is not appropriate to run during `R CMD check`.

## Notes on vignette

The vignette `ViewR-intro.Rmd` sets `eval = FALSE` globally (except for
`sessionInfo()`) because all ViewR examples require an interactive Shiny
session.  The vignette is pre-built so it does not need to execute during
`R CMD check --as-cran`.

## Notes on tests

All tests that exercise `ViewR()` directly use `tryCatch` to swallow
Shiny gadget launch errors (no browser available in the check environment)
and only test the pre-launch argument validation logic.  Tests for the
internal helper functions (filter engine, sort engine, find-and-replace,
code generation) run fully without any interactive session requirement.

## Resubmission notes

This is a resubmission of version 0.2.0 addressing CRAN reviewer feedback:

- Removed the `URL` and `BugReports` fields from DESCRIPTION — the GitHub
  repository is not yet publicly available, so these links returned 404.
- Removed the corresponding `\seealso{}` URL block from `man/ViewR-package.Rd`.
- Removed the GitHub repository URL from `inst/CITATION`.
- Removed the R-CMD-check GitHub Actions badge from `README.md`.

---

*Generated with `usethis::use_cran_comments()`-compatible template.*
