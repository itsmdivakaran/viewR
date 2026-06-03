# =============================================================================
# ViewR -- save_viewr.R
# Export a data frame as a portable, offline ViewR explorer HTML file.
# =============================================================================

#' Save a ViewR explorer to a standalone HTML file
#'
#' Exports any data frame as a fully interactive, offline ViewR explorer that
#' runs in any browser without R or an internet connection.
#'
#' @param data          A `data.frame` or `tibble` to export.
#' @param file          Output `.html` path.
#' @param options       A list created by [viewdt_options()].
#' @param selfcontained Logical. If `TRUE` (default) bundle all assets into a
#'   single file (requires \pkg{pandoc}); if `FALSE`, write a lightweight HTML
#'   file plus a companion `_files/` directory (recommended for large data).
#' @param title         Browser tab title. Default `"ViewR"`.
#' @param dataset_name  Variable name used in generated code. Defaults to the
#'   deparsed `data` expression.
#' @param open          Logical. Open the file in a browser after saving
#'   (interactive sessions only). Default `FALSE`.
#'
#' @return The output file path, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' save_viewdt(mtcars, "mtcars.html", open = TRUE)
#' save_viewdt(iris, "iris.html", selfcontained = FALSE)
#' }
save_viewdt <- function(data,
                        file,
                        options       = viewdt_options(),
                        selfcontained = TRUE,
                        title         = "ViewR",
                        dataset_name  = NULL,
                        open          = FALSE) {

  if (is.null(dataset_name)) {
    dataset_name <- deparse(substitute(data))
    if (length(dataset_name) != 1 || nchar(dataset_name) > 40)
      dataset_name <- "data"
  }

  widget <- viewdt(data, options = options, dataset_name = dataset_name)

  htmlwidgets::saveWidget(
    widget,
    file          = normalizePath(file, mustWork = FALSE),
    selfcontained = selfcontained,
    title         = title
  )

  if (isTRUE(open) && interactive())
    utils::browseURL(normalizePath(file))

  invisible(file)
}
