# ViewR

> **The advanced interactive data table for R.** A modern,
> dependency-free `htmlwidget` that turns any data frame into a fast,
> beautiful, explorable grid — with column analytics, a visual query
> builder, and one-click reproducible code.

``` r
library(ViewR)
viewdt(mtcars)   # that's it
```

[`viewdt()`](https://itsmdivakaran.github.io/viewR/reference/viewdt.md)
is what [`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html)
would look like if it were rebuilt today: every column header is a
**micro-dashboard**, every filter writes **runnable code**, and the
whole thing is a **single portable HTML file** with zero JavaScript
dependencies. It works in the RStudio / Positron Viewer, inside Shiny,
in R Markdown / Quarto, or exported for offline sharing.

------------------------------------------------------------------------

## Why ViewR?

|  | `DT` | `reactable` | **ViewR [`viewdt()`](https://itsmdivakaran.github.io/viewR/reference/viewdt.md)** |
|----|:--:|:--:|:--:|
| Virtualized rendering of large data | ⚠️ | ✅ | ✅ |
| **Compact column dashboards** (badges, mini-histograms, missingness) | ❌ | ❌ | ✅ |
| **Data Insights drawer** (interactive histogram / Pareto) | ❌ | ❌ | ✅ |
| **Visual query builder** (AND/OR, type-aware) | ❌ | ❌ | ✅ |
| **Reproducible code export** (dplyr / base R / SQL) | ❌ | ❌ | ✅ |
| Column picker + global search | ⚠️ | ⚠️ | ✅ |
| Row pinning | ❌ | ❌ | ✅ |
| Light / dark / auto theme | ⚠️ | ⚠️ | ✅ |
| JS framework dependency | jQuery | React | **none (vanilla JS)** |

> ViewR profiles every column **in R** — types, missingness, histogram
> bins, top categories — and ships a compact metadata payload to a lean
> renderer, instead of recomputing statistics in the browser.

------------------------------------------------------------------------

## Highlights

- 🧱 **Virtualized grid** — paints only what’s on screen; sticky
  headers, sticky row index, click-to-pin rows.
- 📊 **Micro-dashboard headers** — data-type badge, mini spark-histogram
  (numeric) or top-category bar (categorical), and a colour-coded
  data-completeness bar with hover tooltip.
- 🔎 **Data Insights drawer** — click any header chart for a full
  interactive SVG histogram or Pareto bar chart, completeness meter, and
  Min/Mean/Median/Max.
- ⚡ **Visual query builder** — multi-condition AND/OR filters with
  type-aware operators (`=`, `<`, `contains`, `is in`, `is NA`, …) and a
  searchable multi-select for categories. Live row/column counter.
- 〈/〉 **Reproducible code** — copy the exact **dplyr**, **base R**, or
  **SQL** for your current filter + visible-column state; your data’s
  variable name is auto-substituted.
- ▦ **Column picker** + global search.
- 🎨 **Light / dark / auto** themes; inline variable labels (haven /
  ADaM ready).
- 💾 **Portable export** —
  [`save_viewdt()`](https://itsmdivakaran.github.io/viewR/reference/save_viewdt.md)
  writes a self-contained offline HTML.

------------------------------------------------------------------------

## Installation

``` r
install.packages("ViewR")
```

------------------------------------------------------------------------

## Quick start

``` r
library(ViewR)

# Open the explorer
viewdt(mtcars)

# Dark theme, hide a column, custom NA placeholder
viewdt(
  iris,
  options = viewdt_options(
    theme          = "dark",
    hidden_columns = "Species",
    na_string      = "—"
  )
)

# Export a portable, offline HTML report
save_viewdt(mtcars, "mtcars.html", open = TRUE)
```

### Inside Shiny

``` r
library(shiny); library(ViewR)
ui <- fluidPage(viewdtOutput("grid", height = "640px"))
server <- function(input, output, session)
  output$grid <- renderViewdt(viewdt(mtcars))
shinyApp(ui, server)
```

### In R Markdown / Quarto

Just call `viewdt(df)` in a chunk — the widget renders inline in the
knitted HTML.

------------------------------------------------------------------------

## The `viewdt()` API

``` r
viewdt(data, options = viewdt_options(), dataset_name = NULL, ...)
```

Everything visual is controlled by
[`viewdt_options()`](https://itsmdivakaran.github.io/viewR/reference/viewdt_options.md):

| Option           | Default  | What it does                          |
|------------------|----------|---------------------------------------|
| `theme`          | `"auto"` | `"auto"`, `"light"`, or `"dark"`      |
| `show_labels`    | `TRUE`   | Inline variable labels in headers     |
| `histograms`     | `TRUE`   | Mini spark-histograms / category bars |
| `missing_bars`   | `TRUE`   | Data-completeness bar per column      |
| `type_badges`    | `TRUE`   | Data-type badges                      |
| `insights`       | `TRUE`   | Sliding Data Insights drawer          |
| `query_builder`  | `TRUE`   | Multi-condition visual filters        |
| `column_picker`  | `TRUE`   | Show/hide columns                     |
| `code_export`    | `TRUE`   | dplyr / base R / SQL generator        |
| `global_search`  | `TRUE`   | Search-all-columns box                |
| `na_string`      | `"NA"`   | Missing-value placeholder             |
| `hidden_columns` | `NULL`   | Columns hidden on first render        |

See
[`vignette("viewdt", package = "ViewR")`](https://itsmdivakaran.github.io/viewR/articles/viewdt.md)
for a full, worked walk-through with live grids.

------------------------------------------------------------------------

## Reproducible code, generated for you

Build a filter in the UI, click **Code**, and copy any of:

``` r
# dplyr
mtcars %>% filter(cyl == 6, mpg > 20) %>% select(mpg, cyl, hp)

# base R
mtcars[mtcars$cyl == 6 & mtcars$mpg > 20, c("mpg", "cyl", "hp")]
```

``` sql
-- SQL
SELECT mpg, cyl, hp FROM mtcars WHERE cyl = 6 AND mpg > 20;
```

------------------------------------------------------------------------

## Also included: the classic `ViewR()` gadget

The original Shiny-gadget viewer/editor is still here for in-session
work — filtering, multi-column sort, an Excel-like editor
(`rhandsontable`), find-and-replace, and live `dplyr` code:

``` r
new_iris <- ViewR(iris, edit = TRUE)   # returns the edited data on Done
```

See [`?ViewR`](https://itsmdivakaran.github.io/viewR/reference/ViewR.md)
and
[`vignette("ViewR-intro", package = "ViewR")`](https://itsmdivakaran.github.io/viewR/articles/ViewR-intro.md).

------------------------------------------------------------------------

## License

MIT © 2024 Mahesh Divakaran
