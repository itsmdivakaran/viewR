# =============================================================================
# ViewR -- viewr_stats.R
# Server-side (R) column profiling. Unlike React-based grids that compute
# statistics in the browser, ViewR profiles every column in R once and ships
# a compact metadata payload to the widget. This keeps the JS engine lean and
# makes the grid responsive on wide, high-row datasets.
# =============================================================================

# Classify a column into one of: "numeric", "datetime", "logical", "character".
.viewr_kind <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXt"))) return("datetime")
  if (is.numeric(x) || is.integer(x))              return("numeric")
  if (is.logical(x))                               return("logical")
  "character"
}

# Short type badge symbol shown in headers.
.viewr_badge <- function(kind) {
  switch(kind,
    numeric  = "#",
    datetime = "\u23f1",  # stopwatch glyph (ASCII-safe source)
    logical  = "T/F",
    "A")                      # character
}

# Profile a single column -> compact list for JSON serialisation.
.viewr_profile_column <- function(x, name, label, hist_bins, top_n) {
  n       <- length(x)
  na_idx  <- is.na(x)
  n_miss  <- sum(na_idx)
  valid   <- x[!na_idx]
  kind    <- .viewr_kind(x)

  out <- list(
    name    = name,
    label   = if (is.null(label) || is.na(label)) "" else as.character(label),
    kind    = kind,
    badge   = .viewr_badge(kind),
    n       = n,
    n_miss  = n_miss,
    miss_pct = if (n > 0) round(100 * n_miss / n, 2) else 0,
    n_unique = length(unique(valid))
  )

  if (kind %in% c("numeric", "datetime") && length(valid) > 0) {
    num <- if (kind == "datetime") as.numeric(valid) else valid
    rng <- range(num)
    out$min    <- rng[1]
    out$max    <- rng[2]
    out$mean   <- mean(num)
    out$median <- stats::median(num)
    # Histogram bins (counts only; JS reconstructs the axis from min/max).
    if (rng[1] == rng[2]) {
      out$hist   <- list(length(num))
      out$breaks <- list(rng[1], rng[2])
    } else {
      h <- graphics_hist(num, hist_bins)
      out$hist   <- as.list(h$counts)
      out$breaks <- as.list(h$breaks)
    }
    out$is_date <- (kind == "datetime")
  } else if (kind %in% c("character", "logical") && length(valid) > 0) {
    tb  <- sort(table(as.character(valid)), decreasing = TRUE)
    top <- utils::head(tb, top_n)
    out$top <- lapply(seq_along(top), function(i) {
      list(
        value = names(top)[i],
        count = as.integer(top[i]),
        pct   = round(100 * as.integer(top[i]) / length(valid), 1)
      )
    })
  }
  out
}

# Lightweight, dependency-free histogram binning (avoids importing graphics).
graphics_hist <- function(x, bins) {
  rng    <- range(x)
  breaks <- seq(rng[1], rng[2], length.out = bins + 1)
  # right-closed bins; force last point inclusive
  idx    <- findInterval(x, breaks, rightmost.closed = TRUE, all.inside = TRUE)
  counts <- tabulate(idx, nbins = bins)
  list(counts = counts, breaks = breaks)
}

# Profile every column of a data frame.
.viewr_profile <- function(data, labels, hist_bins, top_n) {
  nms <- names(data)
  lapply(nms, function(nm) {
    lbl <- if (!is.null(labels) && nm %in% names(labels)) labels[[nm]] else ""
    .viewr_profile_column(data[[nm]], nm, lbl, hist_bins, top_n)
  })
}

# Resolve labels: explicit named vector overrides column "label" attributes.
.viewr_resolve_labels <- function(data, labels) {
  attr_labels <- vapply(data, function(col) {
    l <- attr(col, "label", exact = TRUE)
    if (is.null(l)) NA_character_ else as.character(l)[1]
  }, character(1))
  attr_labels <- attr_labels[!is.na(attr_labels)]
  out <- as.list(attr_labels)
  if (!is.null(labels)) out[names(labels)] <- as.list(labels)
  out
}

# Coerce columns to JSON-friendly atomic vectors (dates -> ISO strings).
.viewr_serialize_data <- function(data) {
  as.data.frame(
    lapply(data, function(col) {
      if (inherits(col, c("Date", "POSIXct", "POSIXt"))) {
        format(col, "%Y-%m-%d")
      } else if (is.factor(col)) {
        as.character(col)
      } else {
        col
      }
    }),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
