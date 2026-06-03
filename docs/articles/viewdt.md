# Get started with viewdt()

[`viewdt()`](https://itsmdivakaran.github.io/viewR/reference/viewdt.md)
is the modern ViewR data explorer: a dependency-free
[`htmlwidget`](https://www.htmlwidgets.org/) built in plain vanilla
JavaScript. It profiles every column **in R** and renders a virtualized
grid with compact micro-dashboard headers, a Data Insights drawer,
a visual query builder, a column picker, and a reproducible code
generator.

It is distinct from the legacy Shiny-gadget
\[[`ViewR()`](https://itsmdivakaran.github.io/viewR/reference/ViewR.md)\],
which remains available for in-session viewing and editing.

## Quick start

The simplest call opens any data frame:

``` r
library(ViewR)
viewdt(mtcars)
```

## Themes, labels, and hidden columns

Every visual feature is controlled through
\[[`viewdt_options()`](https://itsmdivakaran.github.io/viewR/reference/viewdt_options.md)\].
Here we use a dark theme, hide a column, and set a custom missing-value
placeholder:

``` r
viewdt(
  iris,
  options = viewdt_options(
    theme          = "dark",
    hidden_columns = "Species",
    na_string      = "—"
  )
)
```

## Working with labelled (clinical) data

[`viewdt()`](https://itsmdivakaran.github.io/viewR/reference/viewdt.md)
reads `label` column attributes (as produced by **haven** or ADaM
clinical datasets) and shows them inline beneath each column name:

``` r
df <- data.frame(
  AGE = c(54, 61, 47, NA, 72),
  SEX = c("M", "F", "F", "M", "F"),
  BMI = c(24.1, 28.7, 22.3, 31.0, 26.4)
)
attr(df$AGE, "label") <- "Age (years)"
attr(df$SEX, "label") <- "Sex"
attr(df$BMI, "label") <- "Body Mass Index"

viewdt(df, options = viewdt_options(theme = "light"))
```

## Turning features on and off

Disable any panel you do not need — for example a minimal read-only grid
with no query builder, code export, or insights drawer:

``` r
viewdt(
  mtcars,
  options = viewdt_options(
    query_builder = FALSE,
    code_export   = FALSE,
    insights      = FALSE,
    global_search = TRUE
  )
)
```

## What the column headers show

Each header is a small dashboard:

- **Type badge** — `#` numeric, `A` character, `T/F` logical, clock =
  date.
- **Mini distribution** — a spark-histogram for numeric/date columns, or
  a stacked top-categories bar for character columns. Click it to open
  the **Data Insights** drawer.
- **Completeness bar** — green (\>95% complete), amber (50–95%), red
  (\<50%). Hover any `ⓘ` icon for a full metadata card.

## The visual query builder

Click **Filter** to build multi-condition rules combined with `AND` /
`OR`. Operators are type-aware: numeric columns get `<`, `>`, `=`;
character columns add `contains`, `is in`, and `is not in` with a
searchable multi-select. The live row/column counter on the right
updates instantly.

## Reproducible code

Click **Code** to copy the exact pipeline that reproduces your current
filter and visible-column state, in three dialects:

``` r
# dplyr
mtcars %>%
  filter(cyl == 6, mpg > 20) %>%
  select(mpg, cyl, hp)

# base R
mtcars[mtcars$cyl == 6 & mtcars$mpg > 20, c("mpg", "cyl", "hp")]

# SQL
SELECT mpg, cyl, hp
FROM mtcars
WHERE cyl = 6
  AND mpg > 20;
```

The data variable name is auto-substituted from the object you passed to
[`viewdt()`](https://itsmdivakaran.github.io/viewR/reference/viewdt.md).

## Exporting a standalone report

\[[`save_viewdt()`](https://itsmdivakaran.github.io/viewR/reference/save_viewdt.md)\]
writes a fully offline, interactive HTML file that opens in any browser
without R:

``` r
# Single self-contained file (needs pandoc)
save_viewdt(mtcars, "mtcars.html", open = TRUE)

# Lightweight file + companion _files/ dir (best for large data)
save_viewdt(iris, "iris.html", selfcontained = FALSE)
```

## Using viewdt() inside Shiny

``` r
library(shiny)
ui <- fluidPage(viewdtOutput("grid", height = "640px"))
server <- function(input, output, session) {
  output$grid <- renderViewdt(viewdt(mtcars))
}
shinyApp(ui, server)
```
