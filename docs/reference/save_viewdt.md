# Save a ViewR explorer to a standalone HTML file

Exports any data frame as a fully interactive, offline ViewR explorer
that runs in any browser without R or an internet connection.

## Usage

``` r
save_viewdt(
  data,
  file,
  options = viewdt_options(),
  selfcontained = TRUE,
  title = "ViewR",
  dataset_name = NULL,
  open = FALSE
)
```

## Arguments

- data:

  A \`data.frame\` or \`tibble\` to export.

- file:

  Output \`.html\` path.

- options:

  A list created by \[viewdt_options()\].

- selfcontained:

  Logical. If \`TRUE\` (default) bundle all assets into a single file
  (requires pandoc); if \`FALSE\`, write a lightweight HTML file plus a
  companion \`\_files/\` directory (recommended for large data).

- title:

  Browser tab title. Default \`"ViewR"\`.

- dataset_name:

  Variable name used in generated code. Defaults to the deparsed
  \`data\` expression.

- open:

  Logical. Open the file in a browser after saving (interactive sessions
  only). Default \`FALSE\`.

## Value

The output file path, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
save_viewdt(mtcars, "mtcars.html", open = TRUE)
save_viewdt(iris, "iris.html", selfcontained = FALSE)
} # }
```
