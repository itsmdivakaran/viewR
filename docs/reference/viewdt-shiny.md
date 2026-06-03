# Shiny bindings for viewdt

Output and render functions for using \[viewdt()\] within Shiny
applications and interactive R Markdown documents.

## Usage

``` r
viewdtOutput(outputId, width = "100%", height = "600px")

renderViewdt(expr, env = parent.frame(), quoted = FALSE)
```

## Arguments

- outputId:

  Output variable to read from.

- width, height:

  Must be valid CSS units (e.g. \`"100%"\`) or numbers.

- expr:

  An expression that generates a \[viewdt()\] widget.

- env:

  The environment in which to evaluate \`expr\`.

- quoted:

  Is \`expr\` a quoted expression (with \`quote()\`)?

## Value

\`viewdtOutput()\` returns a Shiny output UI element; \`renderViewdt()\`
returns a Shiny render function.
