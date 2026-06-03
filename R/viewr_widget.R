# =============================================================================
# ViewR -- viewr_widget.R
# The modern, dependency-free htmlwidget data explorer (v2.0.0): viewdt().
# =============================================================================

#' Modern interactive data explorer widget
#'
#' Opens a high-performance, self-contained data explorer for a data frame.
#' `viewdt()` profiles every column in R and renders a virtualized grid with
#' \dQuote{Kaggle}-style micro-dashboard headers (data-type badges, mini
#' spark-histograms, and data-completeness bars), hover metadata cards, a
#' sliding Data Insights drawer, a multi-condition visual query builder, a
#' column-visibility picker, and a reproducible code generator (\pkg{dplyr},
#' base R, and SQL).
#'
#' The interface is implemented entirely in dependency-free vanilla JavaScript
#' (no \pkg{React} or build toolchain) and works in the RStudio / Positron
#' Viewer, inside Shiny, in R Markdown / Quarto, or as a portable standalone
#' HTML file via [save_viewdt()].
#'
#' @param data        A `data.frame` or `tibble` to explore.
#' @param options     A list created by [viewdt_options()].
#' @param dataset_name Character used as the data variable name in generated
#'   code. Defaults to the deparsed `data` expression.
#' @param width,height Optional widget dimensions; default to a full-container
#'   responsive layout.
#' @param elementId   Optional explicit element id.
#'
#' @return An `htmlwidget` object.
#' @export
#'
#' @examples
#' \dontrun{
#' viewdt(mtcars)
#' viewdt(iris, options = viewdt_options(theme = "dark", hidden_columns = "Species"))
#' }
viewdt <- function(data,
                   options      = viewdt_options(),
                   dataset_name = NULL,
                   width        = NULL,
                   height       = NULL,
                   elementId    = NULL) {

  if (!is.data.frame(data))
    stop("'data' must be a data.frame or tibble.", call. = FALSE)
  if (!inherits(options, "viewdt_options"))
    stop("'options' must be created by viewdt_options().", call. = FALSE)

  if (is.null(dataset_name)) {
    dataset_name <- deparse(substitute(data))
    if (length(dataset_name) != 1 || nchar(dataset_name) > 40)
      dataset_name <- "data"
  }

  ncells <- as.numeric(nrow(data)) * ncol(data)
  if (ncells > options$max_cells) {
    warning(sprintf(
      "viewdt(): dataset has %.0f cells (> max_cells = %.0f). Rendering may be slow; consider sub-setting.",
      ncells, options$max_cells), call. = FALSE)
  }

  data    <- as.data.frame(data, stringsAsFactors = FALSE)
  labels  <- .viewr_resolve_labels(data, NULL)
  profile <- .viewr_profile(data, labels, options$hist_bins, options$top_n)
  sdata   <- .viewr_serialize_data(data)

  payload <- list(
    data        = jsonlite::toJSON(sdata, dataframe = "columns",
                                   na = "null", digits = 10),
    columns     = as.list(names(sdata)),
    profile     = profile,
    nrow        = nrow(data),
    datasetName = dataset_name,
    options     = options
  )

  htmlwidgets::createWidget(
    name      = "viewdt",
    x         = payload,
    width     = width,
    height    = height,
    package   = "ViewR",
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth  = "100%",
      defaultHeight = 600,
      viewer.fill   = TRUE,
      browser.fill  = TRUE,
      knitr.figure  = FALSE,
      padding       = 0
    )
  )
}

#' Shiny bindings for viewdt
#'
#' Output and render functions for using [viewdt()] within Shiny applications
#' and interactive R Markdown documents.
#'
#' @param outputId Output variable to read from.
#' @param width,height Must be valid CSS units (e.g. `"100\%"`) or numbers.
#' @param expr An expression that generates a [viewdt()] widget.
#' @param env The environment in which to evaluate `expr`.
#' @param quoted Is `expr` a quoted expression (with `quote()`)?
#'
#' @return `viewdtOutput()` returns a Shiny output UI element;
#'   `renderViewdt()` returns a Shiny render function.
#' @name viewdt-shiny
#' @export
viewdtOutput <- function(outputId, width = "100%", height = "600px") {
  htmlwidgets::shinyWidgetOutput(outputId, "viewdt", width, height,
                                 package = "ViewR")
}

#' @rdname viewdt-shiny
#' @export
renderViewdt <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  htmlwidgets::shinyRenderWidget(expr, viewdtOutput, env, quoted = TRUE)
}
