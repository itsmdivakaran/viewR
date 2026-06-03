test_that("viewdt_options returns a classed list with defaults", {
  o <- viewdt_options()
  expect_s3_class(o, "viewdt_options")
  expect_equal(o$theme, "auto")
  expect_true(o$histograms)
  expect_identical(o$hidden_columns, character(0))
})

test_that("viewdt_options validates theme", {
  expect_error(viewdt_options(theme = "neon"))
  expect_equal(viewdt_options(theme = "dark")$theme, "dark")
})

test_that("column profiling produces expected structure", {
  df <- data.frame(
    x = c(1, 2, 3, NA, 5),
    g = c("a", "a", "b", "c", NA),
    stringsAsFactors = FALSE
  )
  prof <- .viewr_profile(df, NULL, hist_bins = 5L, top_n = 5L)
  expect_length(prof, 2)
  px <- prof[[1]]
  expect_equal(px$kind, "numeric")
  expect_equal(px$n_miss, 1)
  expect_equal(px$min, 1)
  expect_equal(px$max, 5)
  pg <- prof[[2]]
  expect_equal(pg$kind, "character")
  expect_true(length(pg$top) >= 1)
  expect_equal(pg$top[[1]]$value, "a")
})

test_that("viewdt builds an htmlwidget", {
  skip_if_not_installed("htmlwidgets")
  w <- viewdt(mtcars)
  expect_s3_class(w, "htmlwidget")
  expect_equal(w$x$nrow, nrow(mtcars))
  expect_true(length(w$x$profile) == ncol(mtcars))
})

test_that("date columns serialize to ISO strings", {
  df <- data.frame(d = as.Date("2024-01-01") + 0:2)
  s <- .viewr_serialize_data(df)
  expect_type(s$d, "character")
  expect_equal(s$d[1], "2024-01-01")
})
