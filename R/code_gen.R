# =============================================================================
# ViewR -- code_gen.R
# Converts the current UI state (filters, sorts, column selection, edits)
# into a clean, copy-pasteable dplyr pipeline.
# =============================================================================


# -- Filter code ---------------------------------------------------------------

#' Generate a dplyr::filter() call from the active filter list.
#' @noRd
.vr_gen_filter_code <- function(filters) {
  if (length(filters) == 0) return(NULL)

  parts <- vapply(filters, function(f) {
    col <- paste0("`", f$column, "`")
    val <- f$value %||% ""

    switch(f$operator,
      "=="          = paste0(col, ' == "', val, '"'),
      "!="          = paste0(col, ' != "', val, '"'),
      ">"           = paste0(col, " > ",  val),
      ">="          = paste0(col, " >= ", val),
      "<"           = paste0(col, " < ",  val),
      "<="          = paste0(col, " <= ", val),
      "contains"    = paste0('grepl("', val, '", ', col,
                             ', ignore.case = TRUE, fixed = TRUE)'),
      "starts with" = paste0('startsWith(tolower(', col, '), "',
                             tolower(val), '")'),
      "ends with"   = paste0('endsWith(tolower(', col, '), "',
                             tolower(val), '")'),
      "is NA"       = paste0("is.na(", col, ")"),
      "is not NA"   = paste0("!is.na(", col, ")"),
      "TRUE"
    )
  }, character(1))

  # Combine with AND / OR
  combined <- parts[1]
  for (i in seq_along(filters)[-1]) {
    sep <- if (identical(filters[[i]]$logic, "OR")) " |\n    " else " &\n    "
    combined <- paste0(combined, sep, parts[i])
  }

  paste0("filter(\n  ", combined, "\n)")
}


# -- Sort code -----------------------------------------------------------------

#' Generate a dplyr::arrange() call from the active sort list.
#' @noRd
.vr_gen_sort_code <- function(sorts) {
  if (length(sorts) == 0) return(NULL)

  parts <- vapply(sorts, function(s) {
    col <- paste0("`", s$column, "`")
    if (identical(s$direction, "desc")) paste0("desc(", col, ")")
    else col
  }, character(1))

  paste0("arrange(", paste(parts, collapse = ", "), ")")
}


# -- Column-select code --------------------------------------------------------

#' Generate a dplyr::select() call for visible columns.
#' @noRd
.vr_gen_select_code <- function(visible_cols, all_cols) {
  if (setequal(visible_cols, all_cols)) return(NULL)   # No change
  if (length(visible_cols) == 0)        return(NULL)

  col_str <- paste(paste0("`", visible_cols, "`"), collapse = ", ")
  paste0("select(", col_str, ")")
}


# -- Find-replace code (inline mutate already built in helpers.R) --------------

# No extra function needed - .vr_find_replace() already returns
# a ready-to-paste mutate() string in result$code.


# -- Assemble full pipeline ----------------------------------------------------

#' Build the complete R code string from the current UI state.
#'
#' @param data_name   character  - name of the source object
#' @param filters     list       - active filter specs
#' @param sorts       list       - active sort specs
#' @param visible_cols character - selected columns
#' @param all_cols    character  - all original columns
#' @param fnr_ops     character  - accumulated find-replace mutate() strings
#' @param edit_ops    character  - accumulated edit operation comments
#' @return Single character string (the generated code).
#' @noRd
.vr_build_code <- function(data_name, filters, sorts,
                            visible_cols, all_cols,
                            fnr_ops   = character(0),
                            edit_ops  = character(0)) {

  steps <- character(0)

  f_code <- .vr_gen_filter_code(filters)
  if (!is.null(f_code)) steps <- c(steps, f_code)

  s_code <- .vr_gen_sort_code(sorts)
  if (!is.null(s_code)) steps <- c(steps, s_code)

  sel_code <- .vr_gen_select_code(visible_cols, all_cols)
  if (!is.null(sel_code)) steps <- c(steps, sel_code)

  if (length(fnr_ops) > 0) steps <- c(steps, fnr_ops)

  if (length(edit_ops) > 0) {
    steps <- c(steps, paste0("# ", edit_ops))
  }

  if (length(steps) == 0) {
    return(paste0("# No operations applied yet.\n",
                  "# Use the Filters, Sort, Columns, Edit, or Find & Replace\n",
                  "# panels to generate reproducible R code here.\n\n",
                  data_name))
  }

  pipe_body <- paste(steps, collapse = " |>\n  ")

  paste0(
    "library(dplyr)\n\n",
    data_name, "_result <- ", data_name, " |>\n",
    "  ", pipe_body
  )
}
