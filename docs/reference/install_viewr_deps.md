# Install All ViewR Dependencies

Checks which required packages are missing from the user's library and
installs them via
[`install.packages`](https://rdrr.io/r/utils/install.packages.html).

## Usage

``` r
install_viewr_deps(ask = TRUE)
```

## Arguments

- ask:

  Logical. If `TRUE` (default), prompt before installing.

## Value

Invisibly returns a character vector of packages that were (or needed to
be) installed.

## Examples

``` r
if (FALSE) { # \dontrun{
install_viewr_deps()
} # }
```
