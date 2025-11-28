library(rtables)

testthat::test_that("cond_rm_facets works", {
  adsl <- ex_adsl
  adsl$colspan_trt <- factor(
    ifelse(adsl[["ARM"]] == "B: Placebo", " ", "Active Study Agent"),
    levels = c("Active Study Agent", " ")
  )
  add_combo <- add_combo_facet(
    "Combined",
    label = "Combined",
    levels = c("A: Drug X", "C: Combination")
  )

  # choose if any facets need to be removed - e.g remove the combined column for placebo
  rm_combo_from_placebo <- cond_rm_facets(
    facets = "Combined",
    ancestor_pos = NA,
    value = " ",
    split = "colspan_trt"
  )

  mysplit <- make_split_fun(post = list(add_combo, rm_combo_from_placebo))

  lyt <- basic_table(
    top_level_section_div = " ",
    show_colcounts = TRUE,
    colcount_format = "N=xx"
  ) %>%
    split_cols_by("colspan_trt", split_fun = trim_levels_in_group("ARM")) %>%
    split_cols_by("ARM", split_fun = mysplit)

  tbl <- build_table(lyt, adsl)
  cols <- make_col_df(tbl, visible_only = TRUE)$name

  expected <- c("A: Drug X", "C: Combination", "Combined", "B: Placebo")

  testthat::expect_identical(cols, expected)

  expect_error(cond_rm_facets())
  expect_error(
    cond_rm_facets(split = "lol", facets = "lol", facets_regex = "lol"),
    "Got both facets and facets_regex"
  )
  expect_error(
    cond_rm_facets(split = "lol"),
    "Must specify facets"
  )
})

testthat::test_that("rm_levels works", {
  adsl <- ex_adsl

  split_fun <- make_split_fun(
    pre = list(rm_levels(excl = c("JPN", "USA", "NGA")))
  )

  lyt <- basic_table() %>%
    split_rows_by("COUNTRY", split_fun = split_fun) %>%
    summarize_row_groups() # for simplicity

  tbl <- build_table(lyt, adsl)

  expected <- setdiff(levels(adsl$COUNTRY), c("JPN", "USA", "NGA"))

  testthat::expect_equal(
    rtables::row.names(tbl),
    expected
  )
})

testthat::test_that("real_add_overall_facet works", {
  adsl <- ex_adsl

  split_fun <- make_split_fun(
    post = list(real_add_overall_facet("Overall", "Overall"))
  )

  lyt <- basic_table() %>%
    split_rows_by("COUNTRY", split_fun = split_fun) %>%
    summarize_row_groups() # for simplicity

  tbl <- build_table(lyt, adsl)

  expected <- c(levels(adsl$COUNTRY), "Overall")

  testthat::expect_equal(
    row.names(tbl),
    expected
  )
})


testthat::test_that("make_combo_splitfun works", {
  adsl <- ex_adsl

  split_fun <- make_combo_splitfun(
    nm = "modified",
    label = "Some Combined Countries",
    levels = c("USA", "CAN")
  )

  lyt <- basic_table() %>%
    split_rows_by("COUNTRY", split_fun = split_fun) %>%
    summarize_row_groups() # for simplicity

  tbl <- build_table(lyt, adsl)

  expected <- "Some Combined Countries"

  testthat::expect_equal(
    row.names(tbl),
    expected
  )

  vals <- sum(table(adsl$COUNTRY)[c("CAN", "USA")])
  denom <- nrow(adsl)

  expected <- list("all obs" = c(vals, vals / denom))

  testthat::expect_equal(
    cell_values(tbl),
    expected
  )
})

testthat::test_that("combine_nonblank works", {
  # function 'combine_nonblank' is similar to 'real_add_overall_facet'
  # but the former one excludes the blank levels when computing the total numbers
  set.seed(123)
  n_blanks <- 10
  adsl <- ex_adsl
  rows_with_blanks <- sample.int(nrow(adsl), n_blanks)
  adsl$COUNTRY <- as.character(adsl$COUNTRY)
  adsl[rows_with_blanks, "COUNTRY"] <- " "
  adsl$COUNTRY <- as.factor(adsl$COUNTRY)

  split_fun <- make_split_fun(post = list(combine_nonblank("Overall", "Overall")))

  lyt <- basic_table() %>%
    split_rows_by("COUNTRY", split_fun = split_fun) %>%
    summarize_row_groups() # for simplicity

  tbl <- build_table(lyt, adsl)

  expected <- c(levels(adsl$COUNTRY), "Overall")

  testthat::expect_equal(
    row.names(tbl),
    expected
  )

  result <- cell_values(tbl["Overall", ])[[1]][1]
  expected <- nrow(adsl) - n_blanks

  testthat::expect_equal(
    result,
    expected
  )
})

testthat::test_that("rm_blank_levels works", {
  set.seed(123)
  n_blanks <- 10
  adsl <- ex_adsl
  rows_with_blanks <- sample.int(nrow(adsl), n_blanks)
  adsl$COUNTRY <- as.character(adsl$COUNTRY)
  adsl[rows_with_blanks, "COUNTRY"] <- " "
  adsl$COUNTRY <- as.factor(adsl$COUNTRY)

  split_fun <- make_split_fun(
    pre = list(rm_blank_levels)
  )

  lyt <- basic_table() %>%
    split_rows_by("COUNTRY") %>%
    summarize_row_groups()
  tbl <- build_table(lyt, adsl)
  row_names_before <- rtables::row.names(tbl)

  lyt <- basic_table() %>%
    split_rows_by("COUNTRY", split_fun = split_fun) %>%
    summarize_row_groups()
  tbl <- build_table(lyt, adsl)
  row_names_after <- rtables::row.names(tbl)


  testthat::expect_equal(
    row_names_before,
    c(" ", row_names_after)
  )
})

testthat::test_that("do_exclude_split works", {
  exclude_levels <- list(
    "COUNTRY" = c("USA", "CAN"),
    "ARM" = c("B: Placebo")
  )

  spl_context1 <- data.frame(
    split = c("STUDY", "COUNTRY"),
    value = c("Study 1", "USA")
  )
  expect_true(do_exclude_split(exclude_levels, spl_context1))

  spl_context2 <- data.frame(
    split = c("STUDY", "COUNTRY"),
    value = c("Study 1", "MEX")
  )
  expect_false(do_exclude_split(exclude_levels, spl_context2))

  spl_context3 <- data.frame(
    split = c("STUDY", "COUNTRY", "ARM"),
    value = c("Study 1", "MEX", "B: Placebo")
  )
  expect_true(do_exclude_split(exclude_levels, spl_context3))
})
