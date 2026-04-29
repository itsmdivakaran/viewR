# ViewR: Interactive Data Viewer, Filter, and Editor

Opens a feature-rich, popup-based Shiny interface for viewing,
exploring, filtering, sorting, editing, and analysing R data frames. All
operations are reflected in real-time as copy-pasteable dplyr code.

## Usage

``` r
ViewR(
  data,
  edit = FALSE,
  popup = TRUE,
  labels = NULL,
  title = NULL,
  viewer = c("dialog", "browser", "pane"),
  generate_code = TRUE,
  theme = c("flatly", "cerulean", "cosmo", "darkly", "lumen", "paper", "readable",
    "sandstone", "simplex", "spacelab", "united", "yeti"),
  max_display = 50000L,
  return_data = TRUE,
  ...
)
```

## Arguments

- data:

  A `data.frame` or `tibble` to view / edit.

- edit:

  Logical. Enable the Excel-like editing tab. Default `FALSE`.

- popup:

  Logical. If `TRUE` (default), open in a popup dialog; if `FALSE`, open
  in the system browser.

- labels:

  Optional named character vector of variable labels. Names must match
  column names of `data`. If `NULL`, labels are read from column
  attributes (e.g.\\ as set by haven).

- title:

  Window title. Defaults to the name of `data`.

- viewer:

  Where to display the interface:

  `"dialog"`

  :   Popup dialog (default when `popup = TRUE`).

  `"browser"`

  :   System web browser.

  `"pane"`

  :   RStudio Viewer pane.

- generate_code:

  Logical. Show the R Code tab. Default `TRUE`.

- theme:

  Bootstrap theme for the UI. One of `"flatly"` (default), `"cerulean"`,
  `"cosmo"`, `"darkly"`, `"lumen"`, `"paper"`, `"readable"`,
  `"sandstone"`, `"simplex"`, `"spacelab"`, `"united"`, `"yeti"`.

- max_display:

  Integer. Maximum rows rendered in the Data View table (for
  performance). Default `50000`.

- return_data:

  Logical. When the user clicks *Done*, return the (possibly edited)
  data frame. Default `TRUE`.

- ...:

  Reserved for future arguments; currently ignored.

## Value

If `return_data = TRUE` and the user clicked *Done*, returns the
modified data frame invisibly. Otherwise returns the original data frame
invisibly.

## Features

- **Data View** - searchable, paginated DT table with optional
  variable-label tooltips and column-level search filters.

- **Sidebar Filters** - add unlimited filter conditions with operators
  `==`, `!=`, `>`, `>=`, `<`, `<=`, *contains*, *starts with*, *ends
  with*, *is NA*, *is not NA*; combine with AND / OR logic.

- **Multi-column Sort** - add multiple sort levels; choose ascending or
  descending per column.

- **Column Visibility** - show/hide any columns via checkboxes.

- **Excel-like Editor** (`edit = TRUE`) - powered by rhandsontable;
  supports in-cell editing, adding rows, and unlimited undo/redo.

- **Find & Replace** - find literal text or regex across one or all
  columns with case-sensitivity and exact-match options; preview changes
  before applying.

- **Variable Info** - data type, N, missing min, max, and sample values
  for every column.

- **R Code Generation** - the *R Code* tab always shows the complete,
  runnable dplyr pipeline for the current state. One click copies it to
  the clipboard.

## Examples

``` r
if (FALSE) { # \dontrun{
## -- Basic view ------------------------------------------------------------
ViewR(mtcars)

## -- Edit mode: returns modified data -------------------------------------
new_iris <- ViewR(iris, edit = TRUE)

## -- Custom labels + dark theme --------------------------------------------
ViewR(mtcars,
      labels = c(mpg = "Miles per Gallon",
                 cyl = "Number of Cylinders",
                 hp  = "Gross Horsepower"),
      theme  = "darkly")

## -- Open in the system browser --------------------------------------------
ViewR(iris, viewer = "browser", generate_code = TRUE)

## -- View a haven-imported dataset (labels read automatically) -------------
# df <- haven::read_sav("my_survey.sav")
# ViewR(df)
} # }
```
