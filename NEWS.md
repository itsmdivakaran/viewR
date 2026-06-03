# ViewR 2.0.0

## Major new architecture: the `viewdt()` data explorer widget

ViewR 2.0.0 introduces an entirely new, dependency-free **`htmlwidget`**
data explorer built from scratch in vanilla JavaScript (no React, no build
toolchain). It is portable: render it in the RStudio/Positron Viewer, inside
Shiny, in R Markdown / Quarto, or export it to a standalone HTML file.

* **`viewdt(data, options = viewdt_options())`** — opens the modern explorer.
* **`viewdt_options()`** — configure theme, labels, hidden columns, NA string,
  visual features, and the viewer-cap safeguard.
* **`save_viewdt()`** — export any data frame to a portable, offline HTML grid.

### Explorer features

* **Virtualized grid** that paints only visible cells, with sticky headers and
  a sticky row-index column; click a row to pin it.
* **Kaggle-style micro-dashboard headers**: data-type badges, mini
  distribution spark-histograms (numeric) / top-category stacked bars
  (categorical), and a colour-coded data-completeness (missingness) bar.
* **Column metadata cards** on the info icon: rows, unique, missing %, and
  Min/Mean/Median/Max or Top-5 categories.
* **Data Insights drawer** with an interactive SVG histogram (numeric) or
  Pareto bar chart (categorical), completeness meter, and descriptive stats.
* **Visual query builder**: multi-condition AND/OR filters with type-aware
  operators and a searchable multi-select for categorical columns.
* **Column visibility picker** and **global search**.
* **Reproducible code generator**: copy-pasteable **dplyr**, **base R**, and
  **SQL** matching the active filter + visible-column state.
* **Light / dark / auto theme**, variable-label display, and custom NA string.

The legacy Shiny-gadget editor remains available as `ViewR()`.

# ViewR 0.2.0

## New features

* **Excel-like editor** (`edit = TRUE`): powered by `rhandsontable`, supports
  inline cell editing, row addition via right-click context menu or the
  *Add Row* button, and unlimited undo / redo.

* **Find & Replace tab**: search for a literal string, regex pattern, or
  exact cell value across one or all columns. A live *Preview* diff table
  shows matched cells before changes are committed.

* **Variable Info tab**: one-row-per-column summary of data type, N,
  missing count, missing %, unique count, min, max, and sample values.

* **R Code tab** (`generate_code = TRUE`): live `dplyr` pipeline that
  reflects all current filters, sorts, column selections, and
  find-and-replace operations. *Copy to clipboard* button included.

* **Multi-condition Filters** with AND / OR logic and eleven operators:
  `==`, `!=`, `>`, `>=`, `<`, `<=`, *contains*, *starts with*,
  *ends with*, *is NA*, *is not NA*.

* **Multi-column Sort**: add as many ordered sort levels as needed, each
  independently ascending or descending.

* **Column visibility panel**: show / hide any column; bulk *All* / *None*
  toggles.

* **Variable labels**: automatically read from column `"label"` attributes
  (e.g. from `haven::read_spss()`); displayed as hoverable tooltips on
  column headers in the Data View table.

* **Twelve Bootstrap themes** via `shinythemes`: `"flatly"`, `"cerulean"`,
  `"cosmo"`, `"darkly"`, `"lumen"`, `"paper"`, `"readable"`,
  `"sandstone"`, `"simplex"`, `"spacelab"`, `"united"`, `"yeti"`.

* `install_viewr_deps()`: helper to detect and install all required
  packages in one call.

* Pipe-friendly: `ViewR()` accepts piped input and returns the working
  data frame invisibly when the user clicks *Done*.

## Bug fixes

* Type coercion after find-and-replace no longer depends on `methods::as()`;
  replaced with an explicit `switch()` over the original column class.

## Internal changes

* Separated UI, server, helpers, and code-generation concerns into
  dedicated source files (`R/ui.R`, `R/server.R`, `R/helpers.R`,
  `R/code_gen.R`).
* Full `testthat` (edition 3) test suite covering filter engine, sort
  engine, find-and-replace, variable info, and code generation.
* Added `vignettes/ViewR-intro.Rmd` introduction vignette.
* Added `_pkgdown.yml` for the package website.
* Hex sticker added to `inst/sticker/`.

---

# ViewR 0.1.0

* Initial release: basic `ViewR()` stub.
