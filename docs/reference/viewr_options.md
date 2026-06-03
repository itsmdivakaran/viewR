# Configure the ViewR data explorer

Builds the option list consumed by \[viewr()\] and \[save_viewr()\].
Every argument has a sensible default, so \`viewr_options()\` with no
arguments returns a fully usable configuration.

## Usage

``` r
viewr_options(
  theme = c("auto", "light", "dark"),
  show_labels = TRUE,
  histograms = TRUE,
  missing_bars = TRUE,
  type_badges = TRUE,
  insights = TRUE,
  query_builder = TRUE,
  column_picker = TRUE,
  code_export = TRUE,
  global_search = TRUE,
  na_string = "NA",
  hidden_columns = NULL,
  page_size = 200L,
  hist_bins = 20L,
  top_n = 10L,
  max_cells = 5e+06
)
```

## Arguments

- theme:

  UI appearance: one of \`"auto"\` (default, follows the host/system
  colour scheme), \`"light"\`, or \`"dark"\`.

- show_labels:

  Logical. Display variable-label attributes (e.g. as set by haven or
  clinical ADaM data) inline in the column headers. Default \`TRUE\`.

- histograms:

  Logical. Render mini spark-histograms / category bars inside the
  column headers. Default \`TRUE\`.

- missing_bars:

  Logical. Render the data-completeness (missingness) bar at the bottom
  of each header. Default \`TRUE\`.

- type_badges:

  Logical. Show data-type badges in headers. Default \`TRUE\`.

- insights:

  Logical. Enable the sliding Data Insights drawer. Default \`TRUE\`.

- query_builder:

  Logical. Enable the multi-condition visual query builder. Default
  \`TRUE\`.

- column_picker:

  Logical. Enable the column-visibility picker. Default \`TRUE\`.

- code_export:

  Logical. Enable the reproducible code generator (dplyr / base R /
  SQL). Default \`TRUE\`.

- global_search:

  Logical. Enable the global search box. Default \`TRUE\`.

- na_string:

  Character. Placeholder shown for missing values. Default \`"NA"\`.

- hidden_columns:

  Character vector of column names hidden on first render. Default
  \`NULL\`.

- page_size:

  Integer. Rows kept in the virtualized DOM buffer. Default \`200L\`.

- hist_bins:

  Integer. Number of bins for numeric histograms. Default \`20L\`.

- top_n:

  Integer. Number of categories profiled for character columns. Default
  \`10L\`.

- max_cells:

  Integer. Soft safeguard; data frames with more than this many cells
  (\`nrow \* ncol\`) trigger a warning. Default \`5e6\`.

## Value

A named list of class \`"viewr_options"\`.

## Examples

``` r
viewr_options(theme = "dark", hidden_columns = c("cyl", "hp"))
#> $theme
#> [1] "dark"
#> 
#> $show_labels
#> [1] TRUE
#> 
#> $histograms
#> [1] TRUE
#> 
#> $missing_bars
#> [1] TRUE
#> 
#> $type_badges
#> [1] TRUE
#> 
#> $insights
#> [1] TRUE
#> 
#> $query_builder
#> [1] TRUE
#> 
#> $column_picker
#> [1] TRUE
#> 
#> $code_export
#> [1] TRUE
#> 
#> $global_search
#> [1] TRUE
#> 
#> $na_string
#> [1] "NA"
#> 
#> $hidden_columns
#> [1] "cyl" "hp" 
#> 
#> $page_size
#> [1] 200
#> 
#> $hist_bins
#> [1] 20
#> 
#> $top_n
#> [1] 10
#> 
#> $max_cells
#> [1] 5e+06
#> 
#> attr(,"class")
#> [1] "viewr_options"
```
