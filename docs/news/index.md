# Changelog

## ViewR 0.2.0

### New features

- **Excel-like editor** (`edit = TRUE`): powered by `rhandsontable`,
  supports inline cell editing, row addition via right-click context
  menu or the *Add Row* button, and unlimited undo / redo.

- **Find & Replace tab**: search for a literal string, regex pattern, or
  exact cell value across one or all columns. A live *Preview* diff
  table shows matched cells before changes are committed.

- **Variable Info tab**: one-row-per-column summary of data type, N,
  missing count, missing %, unique count, min, max, and sample values.

- **R Code tab** (`generate_code = TRUE`): live `dplyr` pipeline that
  reflects all current filters, sorts, column selections, and
  find-and-replace operations. *Copy to clipboard* button included.

- **Multi-condition Filters** with AND / OR logic and eleven operators:
  `==`, `!=`, `>`, `>=`, `<`, `<=`, *contains*, *starts with*, *ends
  with*, *is NA*, *is not NA*.

- **Multi-column Sort**: add as many ordered sort levels as needed, each
  independently ascending or descending.

- **Column visibility panel**: show / hide any column; bulk *All* /
  *None* toggles.

- **Variable labels**: automatically read from column `"label"`
  attributes (e.g. from `haven::read_spss()`); displayed as hoverable
  tooltips on column headers in the Data View table.

- **Twelve Bootstrap themes** via `shinythemes`: `"flatly"`,
  `"cerulean"`, `"cosmo"`, `"darkly"`, `"lumen"`, `"paper"`,
  `"readable"`, `"sandstone"`, `"simplex"`, `"spacelab"`, `"united"`,
  `"yeti"`.

- [`install_viewr_deps()`](https://itsmdivakaran.github.io/ViewR/reference/install_viewr_deps.md):
  helper to detect and install all required packages in one call.

- Pipe-friendly:
  [`ViewR()`](https://itsmdivakaran.github.io/ViewR/reference/ViewR.md)
  accepts piped input and returns the working data frame invisibly when
  the user clicks *Done*.

### Bug fixes

- Type coercion after find-and-replace no longer depends on
  [`methods::as()`](https://rdrr.io/r/methods/as.html); replaced with an
  explicit [`switch()`](https://rdrr.io/r/base/switch.html) over the
  original column class.

### Internal changes

- Separated UI, server, helpers, and code-generation concerns into
  dedicated source files (`R/ui.R`, `R/server.R`, `R/helpers.R`,
  `R/code_gen.R`).
- Full `testthat` (edition 3) test suite covering filter engine, sort
  engine, find-and-replace, variable info, and code generation.
- Added `vignettes/ViewR-intro.Rmd` introduction vignette.
- Added `_pkgdown.yml` for the package website.
- Hex sticker added to `inst/sticker/`.

------------------------------------------------------------------------

## ViewR 0.1.0

- Initial release: basic
  [`ViewR()`](https://itsmdivakaran.github.io/ViewR/reference/ViewR.md)
  stub.
