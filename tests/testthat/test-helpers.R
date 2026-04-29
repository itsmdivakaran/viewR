# =============================================================================
# Tests for R/helpers.R
# Internal functions tested via `:::` notation.
# =============================================================================

# ── Test data ─────────────────────────────────────────────────────────────────

make_df <- function() {
  data.frame(
    name    = c("Alice", "Bob", "Carol", "Dave", NA_character_),
    score   = c(85L, 90L, 78L, 95L, 88L),
    grade   = c("A", "A", "B", "A", "B"),
    passed  = c(TRUE, TRUE, FALSE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
}


# ── .vr_resolve_labels ────────────────────────────────────────────────────────

test_that(".vr_resolve_labels returns empty strings when no labels supplied", {
  df  <- make_df()
  res <- ViewR:::.vr_resolve_labels(df, NULL)
  expect_type(res, "character")
  expect_named(res, names(df))
  expect_true(all(res == ""))
})

test_that(".vr_resolve_labels applies user-supplied labels correctly", {
  df  <- make_df()
  lbl <- c(name = "Participant Name", score = "Test Score")
  res <- ViewR:::.vr_resolve_labels(df, lbl)
  expect_equal(res["name"],  c(name  = "Participant Name"))
  expect_equal(res["score"], c(score = "Test Score"))
  expect_equal(res["grade"], c(grade = ""))
})

test_that(".vr_resolve_labels reads column-attribute labels (haven style)", {
  df       <- make_df()
  attr(df$name,  "label") <- "Full Name"
  attr(df$score, "label") <- "Raw Score"
  res <- ViewR:::.vr_resolve_labels(df, NULL)
  expect_equal(res["name"],  c(name  = "Full Name"))
  expect_equal(res["score"], c(score = "Raw Score"))
})

test_that(".vr_resolve_labels: user labels override column attributes", {
  df <- make_df()
  attr(df$name, "label") <- "Attribute Label"
  res <- ViewR:::.vr_resolve_labels(df, c(name = "User Label"))
  expect_equal(res["name"], c(name = "User Label"))
})

test_that(".vr_resolve_labels ignores labels for non-existent columns", {
  df  <- make_df()
  res <- ViewR:::.vr_resolve_labels(df, c(nonexistent = "Foo"))
  expect_named(res, names(df))
})


# ── .vr_apply_filters ────────────────────────────────────────────────────────

test_that(".vr_apply_filters returns all rows when filter list is empty", {
  df  <- make_df()
  res <- ViewR:::.vr_apply_filters(df, list())
  expect_identical(res, df)
})

test_that(".vr_apply_filters == operator filters correctly", {
  df <- make_df()
  f  <- list(list(id = 1L, column = "grade", operator = "==",
                  value = "A", logic = "AND"))
  res <- ViewR:::.vr_apply_filters(df, f)
  expect_true(all(res$grade == "A"))
  expect_equal(nrow(res), 3L)
})

test_that(".vr_apply_filters != operator works", {
  df <- make_df()
  f  <- list(list(id = 1L, column = "grade", operator = "!=",
                  value = "A", logic = "AND"))
  res <- ViewR:::.vr_apply_filters(df, f)
  expect_true(all(res$grade != "A"))
})

test_that(".vr_apply_filters > operator on numeric column works", {
  df <- make_df()
  f  <- list(list(id = 1L, column = "score", operator = ">",
                  value = "88", logic = "AND"))
  res <- ViewR:::.vr_apply_filters(df, f)
  expect_true(all(res$score > 88L))
  expect_equal(nrow(res), 2L)  # 90, 95
})

test_that(".vr_apply_filters >= operator includes boundary value", {
  df  <- make_df()
  f   <- list(list(id = 1L, column = "score", operator = ">=",
                   value = "88", logic = "AND"))
  res <- ViewR:::.vr_apply_filters(df, f)
  expect_true(all(res$score >= 88L))
  expect_equal(nrow(res), 3L)  # 88, 90, 95
})

test_that(".vr_apply_filters < operator works", {
  df  <- make_df()
  f   <- list(list(id = 1L, column = "score", operator = "<",
                   value = "85", logic = "AND"))
  res <- ViewR:::.vr_apply_filters(df, f)
  expect_true(all(res$score < 85L))
})

test_that(".vr_apply_filters 'contains' does case-insensitive match", {
  df <- make_df()
  f  <- list(list(id = 1L, column = "name", operator = "contains",
                  value = "alice", logic = "AND"))
  res <- ViewR:::.vr_apply_filters(df, f)
  expect_equal(nrow(res), 1L)
  expect_equal(res$name, "Alice")
})

test_that(".vr_apply_filters 'starts with' works", {
  df  <- make_df()
  f   <- list(list(id = 1L, column = "name", operator = "starts with",
                   value = "C", logic = "AND"))
  res <- ViewR:::.vr_apply_filters(df, f)
  expect_equal(res$name, "Carol")
})

test_that(".vr_apply_filters 'ends with' works", {
  df  <- make_df()
  f   <- list(list(id = 1L, column = "name", operator = "ends with",
                   value = "e", logic = "AND"))
  res <- ViewR:::.vr_apply_filters(df, f)
  expect_setequal(res$name, c("Alice", "Dave"))
})

test_that(".vr_apply_filters 'is NA' filters NA rows", {
  df  <- make_df()
  f   <- list(list(id = 1L, column = "name", operator = "is NA",
                   value = "", logic = "AND"))
  res <- ViewR:::.vr_apply_filters(df, f)
  expect_equal(nrow(res), 1L)
  expect_true(is.na(res$name))
})

test_that(".vr_apply_filters 'is not NA' excludes NA rows", {
  df  <- make_df()
  f   <- list(list(id = 1L, column = "name", operator = "is not NA",
                   value = "", logic = "AND"))
  res <- ViewR:::.vr_apply_filters(df, f)
  expect_equal(nrow(res), 4L)
  expect_false(any(is.na(res$name)))
})

test_that(".vr_apply_filters AND logic: both conditions must match", {
  df <- make_df()
  f  <- list(
    list(id = 1L, column = "grade", operator = "==",
         value = "A", logic = "AND"),
    list(id = 2L, column = "score", operator = ">",
         value = "88", logic = "AND")
  )
  res <- ViewR:::.vr_apply_filters(df, f)
  expect_true(all(res$grade == "A" & res$score > 88L))
  expect_equal(nrow(res), 2L)   # Bob(90,A), Dave(95,A)
})

test_that(".vr_apply_filters OR logic: either condition matches", {
  df <- make_df()
  f  <- list(
    list(id = 1L, column = "name", operator = "==",
         value = "Alice", logic = "AND"),
    list(id = 2L, column = "name", operator = "==",
         value = "Bob",   logic = "OR")
  )
  res <- ViewR:::.vr_apply_filters(df, f)
  expect_setequal(res$name, c("Alice", "Bob"))
})


# ── .vr_apply_sorts ──────────────────────────────────────────────────────────

test_that(".vr_apply_sorts returns data unchanged with empty list", {
  df  <- make_df()
  res <- ViewR:::.vr_apply_sorts(df, list())
  expect_identical(res, df)
})

test_that(".vr_apply_sorts ascending sort is correct", {
  df  <- make_df()
  s   <- list(list(id = 1L, column = "score", direction = "asc"))
  res <- ViewR:::.vr_apply_sorts(df, s)
  expect_true(all(diff(res$score[!is.na(res$score)]) >= 0L))
})

test_that(".vr_apply_sorts descending sort is correct", {
  df  <- make_df()
  s   <- list(list(id = 1L, column = "score", direction = "desc"))
  res <- ViewR:::.vr_apply_sorts(df, s)
  expect_true(all(diff(res$score[!is.na(res$score)]) <= 0L))
})

test_that(".vr_apply_sorts character column sorts lexicographically", {
  df  <- make_df()
  s   <- list(list(id = 1L, column = "name", direction = "asc"))
  res <- ViewR:::.vr_apply_sorts(df, s)
  names_no_na <- res$name[!is.na(res$name)]
  expect_equal(names_no_na, sort(names_no_na))
})

test_that(".vr_apply_sorts multi-column sort: primary then secondary", {
  df <- data.frame(
    group = c("B", "A", "A", "B"),
    val   = c(2L,  1L,  3L,  1L),
    stringsAsFactors = FALSE
  )
  s <- list(
    list(id = 1L, column = "group", direction = "asc"),
    list(id = 2L, column = "val",   direction = "desc")
  )
  res <- ViewR:::.vr_apply_sorts(df, s)
  expect_equal(res$group, c("A", "A", "B", "B"))
  expect_equal(res$val,   c(3L, 1L, 2L, 1L))
})

test_that(".vr_apply_sorts puts NAs last", {
  df  <- make_df()
  s   <- list(list(id = 1L, column = "name", direction = "asc"))
  res <- ViewR:::.vr_apply_sorts(df, s)
  expect_true(is.na(res$name[nrow(res)]))
})


# ── .vr_find_replace ─────────────────────────────────────────────────────────

test_that(".vr_find_replace returns unchanged data for empty find string", {
  df  <- make_df()
  res <- ViewR:::.vr_find_replace(df, "__all__", "", "X",
                                   FALSE, FALSE, FALSE, FALSE)
  expect_identical(res$data, df)
  expect_equal(res$n_replaced, 0L)
  expect_null(res$code)
})

test_that(".vr_find_replace basic replacement works", {
  df  <- make_df()
  res <- ViewR:::.vr_find_replace(df, "grade", "A", "Pass",
                                   FALSE, FALSE, FALSE, FALSE)
  expect_equal(res$n_replaced, 3L)
  expect_true(all(res$data$grade[!is.na(res$data$grade)] %in% c("Pass", "B")))
})

test_that(".vr_find_replace preview returns diff rows without modifying data", {
  df  <- make_df()
  res <- ViewR:::.vr_find_replace(df, "grade", "A", "Pass",
                                   FALSE, FALSE, FALSE, TRUE)
  # Original data must not change in preview mode
  expect_identical(res$data, df)
  expect_true(nrow(res$preview) > 0L)
  expect_true(all(c("Row", "Column", "Original", "Replacement")
                  %in% names(res$preview)))
})

test_that(".vr_find_replace case-sensitive: no match on wrong case", {
  df  <- make_df()
  res <- ViewR:::.vr_find_replace(df, "grade", "a", "X",
                                   TRUE, FALSE, FALSE, FALSE)
  expect_equal(res$n_replaced, 0L)
})

test_that(".vr_find_replace case-insensitive: matches regardless of case", {
  df  <- make_df()
  res <- ViewR:::.vr_find_replace(df, "grade", "a", "X",
                                   FALSE, FALSE, FALSE, FALSE)
  expect_equal(res$n_replaced, 3L)
})

test_that(".vr_find_replace regex mode works", {
  df  <- make_df()
  res <- ViewR:::.vr_find_replace(df, "name", "^A", "Z",
                                   FALSE, TRUE, FALSE, FALSE)
  expect_equal(res$n_replaced, 1L)
  expect_true(any(grepl("^Z", res$data$name), na.rm = TRUE))
})

test_that(".vr_find_replace exact match only replaces whole-cell values", {
  df  <- data.frame(x = c("yes", "no", "yes no"), stringsAsFactors = FALSE)
  res <- ViewR:::.vr_find_replace(df, "x", "yes", "Y",
                                   FALSE, FALSE, TRUE, FALSE)
  expect_equal(res$n_replaced, 1L)
  expect_equal(res$data$x, c("Y", "no", "yes no"))
})

test_that(".vr_find_replace generates non-NULL code when replacements occur", {
  df  <- make_df()
  res <- ViewR:::.vr_find_replace(df, "grade", "A", "Pass",
                                   FALSE, FALSE, FALSE, FALSE)
  expect_false(is.null(res$code))
  expect_true(grepl("mutate", res$code))
})

test_that(".vr_find_replace '__all__' applies to all text-like columns", {
  df  <- data.frame(a = c("x", "y"), b = c("x", "z"),
                    stringsAsFactors = FALSE)
  res <- ViewR:::.vr_find_replace(df, "__all__", "x", "Q",
                                   FALSE, FALSE, FALSE, FALSE)
  expect_equal(res$n_replaced, 2L)
  expect_equal(res$data$a[1], "Q")
  expect_equal(res$data$b[1], "Q")
})


# ── .vr_var_info ─────────────────────────────────────────────────────────────

test_that(".vr_var_info returns a data.frame with one row per column", {
  df     <- make_df()
  labels <- ViewR:::.vr_resolve_labels(df, NULL)
  info   <- ViewR:::.vr_var_info(df, labels)
  expect_s3_class(info, "data.frame")
  expect_equal(nrow(info), ncol(df))
})

test_that(".vr_var_info Column field matches names(data)", {
  df     <- make_df()
  labels <- ViewR:::.vr_resolve_labels(df, NULL)
  info   <- ViewR:::.vr_var_info(df, labels)
  expect_equal(info$Column, names(df))
})

test_that(".vr_var_info correctly counts missing values", {
  df          <- make_df()   # 1 NA in 'name'
  labels      <- ViewR:::.vr_resolve_labels(df, NULL)
  info        <- ViewR:::.vr_var_info(df, labels)
  name_row    <- info[info$Column == "name", ]
  expect_equal(name_row$Missing, 1L)
})

test_that(".vr_var_info reports N (non-missing) correctly", {
  df     <- make_df()
  labels <- ViewR:::.vr_resolve_labels(df, NULL)
  info   <- ViewR:::.vr_var_info(df, labels)
  expect_equal(info[info$Column == "score", "N"], 5L)
})

test_that(".vr_var_info type field is populated", {
  df     <- make_df()
  labels <- ViewR:::.vr_resolve_labels(df, NULL)
  info   <- ViewR:::.vr_var_info(df, labels)
  expect_true(all(nzchar(info$Type)))
})

test_that(".vr_var_info Min/Max are populated for numeric columns", {
  df     <- make_df()
  labels <- ViewR:::.vr_resolve_labels(df, NULL)
  info   <- ViewR:::.vr_var_info(df, labels)
  score_row <- info[info$Column == "score", ]
  expect_true(nzchar(score_row$Min))
  expect_true(nzchar(score_row$Max))
})


# ── .vr_re_escape ─────────────────────────────────────────────────────────────

test_that(".vr_re_escape escapes regex special characters", {
  raw     <- "1.5 (approx)"
  escaped <- ViewR:::.vr_re_escape(raw)
  # The escaped version should not produce matches against random strings
  expect_equal(regmatches("x", regexpr(escaped, "x")), character(0))
  # It should match the literal string
  expect_equal(regmatches(raw, regexpr(escaped, raw)), raw)
})
