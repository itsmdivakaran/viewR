# =============================================================================
# ViewR -- viewr_options.R
# Configuration helper for the viewdt() data explorer widget.
# =============================================================================

#' Configure the ViewR data explorer
#'
#' Builds the option list consumed by [viewdt()] and [save_viewdt()]. Every
#' argument has a sensible default, so `viewdt_options()` with no arguments
#' returns a fully usable configuration.
#'
#' @param theme            UI appearance: one of `"auto"` (default, follows the
#'   host/system colour scheme), `"light"`, or `"dark"`.
#' @param show_labels      Logical. Display variable-label attributes (e.g. as
#'   set by \pkg{haven} or clinical ADaM data) inline in the column headers.
#'   Default `TRUE`.
#' @param histograms       Logical. Render mini spark-histograms / category
#'   bars inside the column headers. Default `TRUE`.
#' @param missing_bars     Logical. Render the data-completeness (missingness)
#'   bar at the bottom of each header. Default `TRUE`.
#' @param type_badges      Logical. Show data-type badges in headers.
#'   Default `TRUE`.
#' @param insights         Logical. Enable the sliding Data Insights drawer.
#'   Default `TRUE`.
#' @param query_builder    Logical. Enable the multi-condition visual query
#'   builder. Default `TRUE`.
#' @param column_picker    Logical. Enable the column-visibility picker.
#'   Default `TRUE`.
#' @param code_export      Logical. Enable the reproducible code generator
#'   (dplyr / base R / SQL). Default `TRUE`.
#' @param global_search    Logical. Enable the global search box. Default `TRUE`.
#' @param na_string        Character. Placeholder shown for missing values.
#'   Default `"NA"`.
#' @param hidden_columns   Character vector of column names hidden on first
#'   render. Default `NULL`.
#' @param page_size        Integer. Rows kept in the virtualized DOM buffer.
#'   Default `200L`.
#' @param hist_bins        Integer. Number of bins for numeric histograms.
#'   Default `20L`.
#' @param top_n            Integer. Number of categories profiled for character
#'   columns. Default `10L`.
#' @param max_cells        Integer. Soft safeguard; data frames with more than
#'   this many cells (`nrow * ncol`) trigger a warning. Default `5e6`.
#'
#' @return A named list of class `"viewdt_options"`.
#' @export
#'
#' @examples
#' viewdt_options(theme = "dark", hidden_columns = c("cyl", "hp"))
viewdt_options <- function(theme          = c("auto", "light", "dark"),
                          show_labels    = TRUE,
                          histograms     = TRUE,
                          missing_bars   = TRUE,
                          type_badges    = TRUE,
                          insights       = TRUE,
                          query_builder  = TRUE,
                          column_picker  = TRUE,
                          code_export    = TRUE,
                          global_search  = TRUE,
                          na_string      = "NA",
                          hidden_columns = NULL,
                          page_size      = 200L,
                          hist_bins      = 20L,
                          top_n          = 10L,
                          max_cells      = 5e6) {

  theme <- match.arg(theme)

  stopifnot(
    is.logical(show_labels), is.logical(histograms), is.logical(missing_bars),
    is.logical(type_badges), is.logical(insights), is.logical(query_builder),
    is.logical(column_picker), is.logical(code_export), is.logical(global_search)
  )

  structure(
    list(
      theme          = theme,
      show_labels    = isTRUE(show_labels),
      histograms     = isTRUE(histograms),
      missing_bars   = isTRUE(missing_bars),
      type_badges    = isTRUE(type_badges),
      insights       = isTRUE(insights),
      query_builder  = isTRUE(query_builder),
      column_picker  = isTRUE(column_picker),
      code_export    = isTRUE(code_export),
      global_search  = isTRUE(global_search),
      na_string      = as.character(na_string)[1],
      hidden_columns = if (is.null(hidden_columns)) character(0)
                       else as.character(hidden_columns),
      page_size      = as.integer(page_size),
      hist_bins      = as.integer(hist_bins),
      top_n          = as.integer(top_n),
      max_cells      = as.numeric(max_cells)
    ),
    class = "viewdt_options"
  )
}
