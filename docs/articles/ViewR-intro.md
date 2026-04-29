# Getting Started with ViewR

## Overview

**ViewR** brings spreadsheet-style data exploration directly into your R
workflow. A single function call opens a polished, popup-based interface
that lets you filter, sort, search, edit, and understand any
`data.frame` — and automatically writes the equivalent `dplyr` code so
you never lose reproducibility.

``` r
library(ViewR)
ViewR(mtcars)
```

------------------------------------------------------------------------

## Installation

Install the release version from CRAN:

``` r
install.packages("ViewR")
```

Or install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("imaheshdivakaran/ViewR")
```

Run this helper to check and install all required dependencies:

``` r
install_viewr_deps()
```

------------------------------------------------------------------------

## The `ViewR()` function

The entire interface is controlled through one function:

``` r
ViewR(
  data,
  edit          = FALSE,   # enable Excel-like editing
  popup         = TRUE,    # open as modal dialog
  labels        = NULL,    # named vector of variable labels
  title         = NULL,    # custom window title
  viewer        = "dialog",# "dialog", "browser", or "pane"
  generate_code = TRUE,    # show R Code tab
  theme         = "flatly",# Bootstrap theme
  max_display   = 50000L,  # row cap for rendering
  return_data   = TRUE     # return (possibly edited) data on close
)
```

[`ViewR()`](https://itsmdivakaran.github.io/ViewR/reference/ViewR.md) is
**pipe-friendly** and returns the (possibly modified) data frame
invisibly when the user clicks *Done*:

``` r
library(dplyr)

clean_data <- mtcars |>
  filter(cyl >= 4) |>
  ViewR(edit = TRUE)   # open popup; result assigned on Done
```

------------------------------------------------------------------------

## Walkthrough of the interface

### Sidebar

The left-hand sidebar provides three action panels that update the data
display in real time.

#### Filters

Click the green **+** button to add a filter condition. Each row has:

| Control | Options |
|----|----|
| Column | any column in the data |
| Operator | `==`, `!=`, `>`, `>=`, `<`, `<=`, *contains*, *starts with*, *ends with*, *is NA*, *is not NA* |
| Value | free-text input |
| Logic | **AND** (default) or **OR** (shown from the second row onward) |

You can add as many filter rows as you need. Click **Clear all** to
remove all filters at once.

#### Sort

Add one or more sort levels. Each level has a column and a direction
(ascending ↑ or descending ↓). Levels are applied in order — the first
is the primary sort key, the second is the tiebreaker, and so on.

#### Columns

Tick or untick columns to show or hide them. The **All** / **None**
links act as bulk toggles.

------------------------------------------------------------------------

### Main tabs

#### Data

The main data table is powered by the **DT** package. It supports:

- **Global search** (top-right of the table)
- **Per-column filter boxes** below each header
- Clickable column headers for one-click ascending/descending sort
- Configurable page length (use the *Rows per page* slider in the
  sidebar)

If you supply `labels` (or use a dataset imported with **haven**), each
column header will show an ⓘ tooltip with the full label when you hover.

#### Edit *(requires `edit = TRUE`)*

An Excel-like grid powered by **rhandsontable**. You can:

- Click any cell and type to edit its value directly
- Right-click a row header for insert / remove options
- Use the **Add Row** button to append a blank row
- **Undo** and **Redo** (unlimited) step through the change history

Changes are committed to the returned data frame when you click
**Done**.

> **Tip:** Clear all filters and sorts before editing to ensure row
> positions map correctly back to the original data.

#### Find & Replace

Search for any text pattern — literal or regex — and replace it in one
or all columns. The workflow is:

1.  Choose a column (or *All columns*) from the dropdown.
2.  Type the **Find** string and the **Replace with** string.
3.  Optionally tick *Case sensitive*, *Regex*, or *Exact cell match*.
4.  Click **Preview** to see a diff table of affected cells.
5.  Click **Apply** to commit the replacement.

#### Variable Info

A summary table for every column in the current working data:

| Column        | Description                                |
|---------------|--------------------------------------------|
| Type          | R class (character, integer, numeric, …)   |
| N             | Count of non-missing values                |
| Missing       | Count of NA values                         |
| Missing %     | Percentage missing (colour-bar visualised) |
| Unique        | Number of distinct values                  |
| Min / Max     | For numeric columns                        |
| Sample values | Up to 4 example values                     |

#### R Code

Every action you perform — filtering, sorting, showing/hiding columns,
find-and-replace — is translated into a **dplyr** pipeline and displayed
here in real time:

``` r
library(dplyr)

mtcars_result <- mtcars |>
  filter(
    `cyl` == "6"
  ) |>
  arrange(desc(`mpg`)) |>
  select(`mpg`, `cyl`, `hp`, `wt`)
```

Click **Copy to clipboard** to paste the code directly into your script.
Click **Reset** to clear the history of find-replace and edit operations
(filter/sort/column code is always live and cannot be cleared
independently).

------------------------------------------------------------------------

## Variable labels

ViewR automatically reads variable labels stored as column attributes,
as produced by **haven**, **labelled**, or **Hmisc**:

``` r
# df <- haven::read_sav("my_survey.sav")
# ViewR(df)   # labels appear as tooltips automatically
```

You can also supply labels manually:

``` r
ViewR(mtcars,
      labels = c(
        mpg  = "Miles per Gallon",
        cyl  = "Number of Cylinders",
        disp = "Displacement (cu.in.)",
        hp   = "Gross Horsepower",
        wt   = "Weight (1000 lbs)"
      ))
```

------------------------------------------------------------------------

## Themes

Choose from any Bootstrap theme supported by **shinythemes**:

``` r
ViewR(iris, theme = "darkly")    # dark
ViewR(iris, theme = "cerulean")  # light blue
ViewR(iris, theme = "cosmo")     # clean & minimal
ViewR(iris, theme = "sandstone") # warm beige
```

------------------------------------------------------------------------

## Opening in the browser or Viewer pane

By default ViewR opens as a modal dialog inside RStudio. Use the
`viewer` argument to change this:

``` r
# Open in the system browser (useful for large datasets)
ViewR(iris, viewer = "browser")

# Open in the RStudio Viewer pane
ViewR(iris, viewer = "pane")

# Or equivalently, set popup = FALSE (defaults to browser)
ViewR(iris, popup = FALSE)
```

------------------------------------------------------------------------

## Editing data and returning results

When `edit = TRUE`, clicking **Done** returns the modified data frame:

``` r
# Open the editor; assign the result
corrected <- ViewR(survey_data, edit = TRUE)

# 'corrected' now contains all inline edits + any find-replace operations
head(corrected)
```

If you click **Cancel**, the original unmodified data frame is returned.

------------------------------------------------------------------------

## Full example: survey data workflow

``` r
library(ViewR)

# Load a dataset (here we use the built-in survey proxy)
data(Titanic)
titanic_df <- as.data.frame(Titanic)

# Step 1 — Explore: inspect variable info and browse the data
ViewR(titanic_df,
      labels = c(Class    = "Passenger Class",
                 Sex      = "Passenger Sex",
                 Age      = "Age Group",
                 Survived = "Survival Status",
                 Freq     = "Count"),
      theme  = "flatly")

# Step 2 — Clean: use Find & Replace inside the popup to
#   standardise "Male" -> "M", "Female" -> "F"

# Step 3 — Filter & export: the R Code tab will have generated:
#   titanic_df_result <- titanic_df |>
#     filter(`Survived` == "Yes") |>
#     arrange(desc(`Freq`))
# Copy, paste, and run!

# Step 4 — Edit specific cells if needed
titanic_clean <- ViewR(titanic_df, edit = TRUE)
```

------------------------------------------------------------------------

## Session info

``` r
sessionInfo()
#> R version 4.5.2 (2025-10-31)
#> Platform: aarch64-apple-darwin20
#> Running under: macOS Tahoe 26.4.1
#> 
#> Matrix products: default
#> BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
#> LAPACK: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1
#> 
#> locale:
#> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#> 
#> time zone: Asia/Kolkata
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices datasets  utils     methods   base     
#> 
#> loaded via a namespace (and not attached):
#>  [1] digest_0.6.39     desc_1.4.3        R6_2.6.1          fastmap_1.2.0    
#>  [5] xfun_0.57         cachem_1.1.0      knitr_1.51        htmltools_0.5.9  
#>  [9] rmarkdown_2.31    lifecycle_1.0.5   cli_3.6.5         sass_0.4.10      
#> [13] pkgdown_2.2.0     textshaping_1.0.5 jquerylib_0.1.4   renv_1.1.8       
#> [17] systemfonts_1.3.2 compiler_4.5.2    rstudioapi_0.18.0 tools_4.5.2      
#> [21] ragg_1.5.2        bslib_0.10.0      evaluate_1.0.5    yaml_2.3.12      
#> [25] otel_0.2.0        jsonlite_2.0.0    htmlwidgets_1.6.4 rlang_1.1.7      
#> [29] fs_2.0.1
```
