# =============================================================================
# Tests for R/code_gen.R
# =============================================================================

# ── .vr_gen_filter_code ───────────────────────────────────────────────────────

test_that(".vr_gen_filter_code returns NULL for empty list", {
  expect_null(ViewR:::.vr_gen_filter_code(list()))
})

test_that(".vr_gen_filter_code generates == expression", {
  f   <- list(list(id = 1L, column = "grade", operator = "==",
                   value = "A", logic = "AND"))
  out <- ViewR:::.vr_gen_filter_code(f)
  expect_false(is.null(out))
  expect_true(grepl("filter", out))
  expect_true(grepl("`grade`", out))
  expect_true(grepl("==", out))
  expect_true(grepl('"A"', out))
})

test_that(".vr_gen_filter_code generates != expression", {
  f   <- list(list(id = 1L, column = "x", operator = "!=",
                   value = "z", logic = "AND"))
  out <- ViewR:::.vr_gen_filter_code(f)
  expect_true(grepl("!=", out))
})

test_that(".vr_gen_filter_code generates > expression without quotes", {
  f   <- list(list(id = 1L, column = "score", operator = ">",
                   value = "80", logic = "AND"))
  out <- ViewR:::.vr_gen_filter_code(f)
  expect_true(grepl("`score` > 80", out))
})

test_that(".vr_gen_filter_code 'contains' generates grepl", {
  f   <- list(list(id = 1L, column = "name", operator = "contains",
                   value = "ali", logic = "AND"))
  out <- ViewR:::.vr_gen_filter_code(f)
  expect_true(grepl("grepl", out))
  expect_true(grepl("ali", out))
  expect_true(grepl("ignore.case", out))
})

test_that(".vr_gen_filter_code 'starts with' generates startsWith", {
  f   <- list(list(id = 1L, column = "name", operator = "starts with",
                   value = "Al", logic = "AND"))
  out <- ViewR:::.vr_gen_filter_code(f)
  expect_true(grepl("startsWith", out) || grepl("^", out, fixed = TRUE))
})

test_that(".vr_gen_filter_code 'is NA' generates is.na", {
  f   <- list(list(id = 1L, column = "x", operator = "is NA",
                   value = "", logic = "AND"))
  out <- ViewR:::.vr_gen_filter_code(f)
  expect_true(grepl("is.na", out))
})

test_that(".vr_gen_filter_code 'is not NA' generates !is.na", {
  f   <- list(list(id = 1L, column = "x", operator = "is not NA",
                   value = "", logic = "AND"))
  out <- ViewR:::.vr_gen_filter_code(f)
  expect_true(grepl("!is.na", out))
})

test_that(".vr_gen_filter_code combines two AND conditions with &", {
  f <- list(
    list(id = 1L, column = "a", operator = "==", value = "x", logic = "AND"),
    list(id = 2L, column = "b", operator = "==", value = "y", logic = "AND")
  )
  out <- ViewR:::.vr_gen_filter_code(f)
  expect_true(grepl("&", out))
})

test_that(".vr_gen_filter_code combines AND+OR conditions correctly", {
  f <- list(
    list(id = 1L, column = "a", operator = "==", value = "x", logic = "AND"),
    list(id = 2L, column = "b", operator = "==", value = "y", logic = "OR")
  )
  out <- ViewR:::.vr_gen_filter_code(f)
  expect_true(grepl("\\|", out))
})


# ── .vr_gen_sort_code ────────────────────────────────────────────────────────

test_that(".vr_gen_sort_code returns NULL for empty list", {
  expect_null(ViewR:::.vr_gen_sort_code(list()))
})

test_that(".vr_gen_sort_code ascending produces bare column reference", {
  s   <- list(list(id = 1L, column = "score", direction = "asc"))
  out <- ViewR:::.vr_gen_sort_code(s)
  expect_true(grepl("arrange", out))
  expect_true(grepl("`score`", out))
  expect_false(grepl("desc", out))
})

test_that(".vr_gen_sort_code descending wraps column in desc()", {
  s   <- list(list(id = 1L, column = "score", direction = "desc"))
  out <- ViewR:::.vr_gen_sort_code(s)
  expect_true(grepl("desc\\(`score`\\)", out))
})

test_that(".vr_gen_sort_code multi-column sort lists all columns", {
  s <- list(
    list(id = 1L, column = "grade", direction = "asc"),
    list(id = 2L, column = "score", direction = "desc")
  )
  out <- ViewR:::.vr_gen_sort_code(s)
  expect_true(grepl("`grade`", out))
  expect_true(grepl("desc\\(`score`\\)", out))
})


# ── .vr_gen_select_code ──────────────────────────────────────────────────────

test_that(".vr_gen_select_code returns NULL when column sets are identical", {
  cols <- c("a", "b", "c")
  expect_null(ViewR:::.vr_gen_select_code(cols, cols))
})

test_that(".vr_gen_select_code returns NULL for empty visible_cols", {
  expect_null(ViewR:::.vr_gen_select_code(character(0), c("a", "b")))
})

test_that(".vr_gen_select_code generates select() with backtick-quoted names", {
  out <- ViewR:::.vr_gen_select_code(c("a", "b"), c("a", "b", "c"))
  expect_true(grepl("select", out))
  expect_true(grepl("`a`", out))
  expect_true(grepl("`b`", out))
  expect_false(grepl("`c`", out))
})

test_that(".vr_gen_select_code handles column names with spaces", {
  out <- ViewR:::.vr_gen_select_code(c("col one"), c("col one", "col two"))
  expect_true(grepl("`col one`", out))
})


# ── .vr_build_code ────────────────────────────────────────────────────────────

test_that(".vr_build_code returns placeholder when no operations", {
  out <- ViewR:::.vr_build_code(
    data_name    = "mydata",
    filters      = list(),
    sorts        = list(),
    visible_cols = c("a", "b"),
    all_cols     = c("a", "b")
  )
  expect_true(grepl("No operations", out, ignore.case = TRUE))
})

test_that(".vr_build_code includes library(dplyr) header when ops present", {
  f <- list(list(id = 1L, column = "x", operator = "==",
                 value = "y", logic = "AND"))
  out <- ViewR:::.vr_build_code(
    data_name    = "df",
    filters      = f,
    sorts        = list(),
    visible_cols = c("x"),
    all_cols     = c("x")
  )
  expect_true(grepl("library\\(dplyr\\)", out))
  expect_true(grepl("df_result", out))
})

test_that(".vr_build_code assembles filter + sort + select in order", {
  f   <- list(list(id = 1L, column = "g", operator = "==",
                   value = "A", logic = "AND"))
  s   <- list(list(id = 1L, column = "v", direction = "asc"))
  out <- ViewR:::.vr_build_code(
    data_name    = "dat",
    filters      = f,
    sorts        = s,
    visible_cols = c("g"),
    all_cols     = c("g", "v")
  )
  # filter() should appear before arrange() in the pipeline
  filter_pos  <- regexpr("filter",  out)[[1]]
  arrange_pos <- regexpr("arrange", out)[[1]]
  select_pos  <- regexpr("select",  out)[[1]]
  expect_true(filter_pos  < arrange_pos)
  expect_true(arrange_pos < select_pos)
})

test_that(".vr_build_code appends find-replace mutate ops", {
  fnr_op <- 'mutate(\n  `x` = gsub("old", "new", `x`)\n)'
  out    <- ViewR:::.vr_build_code(
    data_name    = "dat",
    filters      = list(),
    sorts        = list(),
    visible_cols = c("x"),
    all_cols     = c("x"),
    fnr_ops      = fnr_op
  )
  expect_true(grepl("mutate", out))
  expect_true(grepl("gsub", out))
})

test_that(".vr_build_code wraps edit ops in comments", {
  out <- ViewR:::.vr_build_code(
    data_name    = "dat",
    filters      = list(),
    sorts        = list(),
    visible_cols = c("x"),
    all_cols     = c("x"),
    edit_ops     = "Row added"
  )
  expect_true(grepl("# Row added", out))
})
