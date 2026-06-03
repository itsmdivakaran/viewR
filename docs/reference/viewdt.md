# Modern interactive data explorer widget

Opens a high-performance, self-contained data explorer for a data frame.
\`viewdt()\` profiles every column in R and renders a virtualized grid
with “Kaggle”-style micro-dashboard headers (data-type badges, mini
spark-histograms, and data-completeness bars), hover metadata cards, a
sliding Data Insights drawer, a multi-condition visual query builder, a
column-visibility picker, and a reproducible code generator (dplyr, base
R, and SQL).

## Usage

``` r
viewdt(
  data,
  options = viewdt_options(),
  dataset_name = NULL,
  width = NULL,
  height = NULL,
  elementId = NULL
)
```

## Arguments

- data:

  A \`data.frame\` or \`tibble\` to explore.

- options:

  A list created by \[viewdt_options()\].

- dataset_name:

  Character used as the data variable name in generated code. Defaults
  to the deparsed \`data\` expression.

- width, height:

  Optional widget dimensions; default to a full-container responsive
  layout.

- elementId:

  Optional explicit element id.

## Value

An \`htmlwidget\` object.

## Details

The interface is implemented entirely in dependency-free vanilla
JavaScript (no React or build toolchain) and works in the RStudio /
Positron Viewer, inside Shiny, in R Markdown / Quarto, or as a portable
standalone HTML file via \[save_viewdt()\].

## Examples

``` r
if (FALSE) { # \dontrun{
viewdt(mtcars)
viewdt(iris, options = viewdt_options(theme = "dark", hidden_columns = "Species"))
} # }
```
