# Shiny bindings for viewr

Output and render functions for using \[viewr()\] within Shiny
applications and interactive R Markdown documents.

## Usage

``` r
viewrOutput(outputId, width = "100%", height = "600px")

renderViewr(expr, env = parent.frame(), quoted = FALSE)
```

## Arguments

- outputId:

  Output variable to read from.

- width, height:

  Must be valid CSS units (e.g. \`"100%"\`) or numbers.

- expr:

  An expression that generates a \[viewr()\] widget.

- env:

  The environment in which to evaluate \`expr\`.

- quoted:

  Is \`expr\` a quoted expression (with \`quote()\`)?

## Value

\`viewrOutput()\` returns a Shiny output UI element; \`renderViewr()\`
returns a Shiny render function.
