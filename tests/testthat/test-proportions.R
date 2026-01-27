test_that("s_proportion_factor works as expected", {
  result <- s_proportion_factor(
    factor(c("a", "b", "a")),
    .alt_df = data.frame(a = 1:5)
  )
  expect_snapshot(result)
})

test_that("s_proportion_factor shows optional total row on top", {
  result <- s_proportion_factor(
    x = factor(c("a", "b", "a")),
    .alt_df = data.frame(a = 1:5),
    show_total = "top"
  )
  expect_snapshot(result)
})

test_that("s_proportion_factor shows optional total row in bottom", {
  result <- s_proportion_factor(
    x = factor(c("a", "b", "a")),
    .alt_df = data.frame(a = 1:5),
    show_total = "bottom",
    total_label = "bla"
  )
  expect_snapshot(result)
})

test_that("s_proportion_factor optionally uses number of non-missing levels as total", {
  result <- s_proportion_factor(
    x = factor(c("a", "b", NA, "a")),
    .alt_df = data.frame(a = 1:10),
    show_total = "bottom",
    total_label = "foo",
    use_alt_counts = FALSE
  )
  expect_snapshot(result)
})

test_that("s_proportion_logical works as expected", {
  result <- s_proportion_logical(
    c(TRUE, FALSE, TRUE),
    .alt_df = data.frame(a = 1:5)
  )
  expect_snapshot(result)
})

test_that("c_proportion_logical works as expected", {
  result <- c_proportion_logical(
    x = c(TRUE, FALSE, TRUE),
    labelstr = "A",
    label_fstr = "Group %s",
    format = jjcsformat_count_fraction,
    .N_col = 5
  )
  expect_snapshot(result)
})

test_that("h_get_design_mat works as expected", {
  result <- h_get_design_mat(
    df = data.frame(a = factor(c("a", "b", "a"))),
    .var = "a"
  )
  expected <- matrix(
    data = c(
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      TRUE,
      FALSE
    ),
    byrow = TRUE,
    nrow = 3,
    ncol = 2
  )
  expect_equal(result, expected, ignore_attr = TRUE)
  expect_identical(colnames(result), c("a", "b"))
})

test_that("a_proportion_ci_logical works as expected", {
  result <- a_proportion_ci_logical(
    x = DM$SEX == "F",
    .alt_df = DM,
    conf_level = 0.95,
    formats = list(prop_ci = jjcsformat_xx("xx.xx% - xx.xx%")),
    method = "wald"
  )
  expect_s3_class(result, "CellValue")
  expect_snapshot(result)
})

test_that("a_proportion_ci_factor works as expected", {
  result <- a_proportion_ci_factor(
    df = DM,
    .var = "SEX",
    .alt_df = DM,
    conf_level = 0.95,
    formats = list(prop_ci = jjcsformat_xx("xx.x%, xx.x%")),
    method = "clopper-pearson"
  )
  expect_s3_class(result, "RowsVerticalSection")
  expect_snapshot(result)
})

test_that("prop_split_fun works as expected", {
  result <- basic_table() %>%
    split_cols_by("ID", split_fun = prop_split_fun) |>
    build_table(formatters::DM)
  expect_snapshot(result)
})

test_that("prop_table_afun works as expected with total row", {
  formats <- list(
    n = "xx",
    percent = "xx.xx",
    cum_percent = "xx.xxxx"
  )

  fake_spl_context <- data.frame(
    cur_col_split_val = I(list(c(ARM = "A: Drug X", percent = "percent")))
  )
  result <- prop_table_afun(
    x = factor(c("a", "b", "a"), levels = c("b", "a")),
    .spl_context = fake_spl_context,
    formats = formats,
    add_total_level = TRUE
  )
  expect_snapshot(result)

  fake_spl_context <- data.frame(
    cur_col_split_val = I(list(c(n = "n")))
  )
  result <- prop_table_afun(
    x = factor(c("a", "b", "a"), levels = c("b", "a")),
    .spl_context = fake_spl_context,
    formats = formats,
    add_total_level = TRUE
  )
  expect_snapshot(result)

  fake_spl_context <- data.frame(
    cur_col_split_val = I(list(c(
      x = "a",
      b = "c",
      cum_percent = "cum_percent"
    )))
  )
  result <- prop_table_afun(
    x = factor(c("a", "b", "a"), levels = c("b", "a")),
    .spl_context = fake_spl_context,
    formats = formats,
    add_total_level = TRUE
  )
  expect_snapshot(result)

  fake_spl_context <- data.frame(
    cur_col_split_val = I(list(c(x = "a", b = "c", bla = "foo")))
  )
  expect_error(
    prop_table_afun(
      x = factor(c("a", "b", "a"), levels = c("b", "a")),
      .spl_context = fake_spl_context,
      formats = formats,
      add_total_level = TRUE
    ),
    "unexpected"
  )
})

test_that("prop_table_afun works as expected, by default without total row", {
  formats <- list(
    n = "xx",
    percent = "xx.xx",
    cum_percent = "xx.xxxx"
  )

  fake_spl_context <- data.frame(
    cur_col_split_val = I(list(c(ARM = "A: Drug X", percent = "percent")))
  )
  result <- prop_table_afun(
    x = factor(c("a", "b", "a"), levels = c("b", "a")),
    .spl_context = fake_spl_context,
    formats = formats
  )
  expect_snapshot(result)

  fake_spl_context <- data.frame(
    cur_col_split_val = I(list(c(n = "n")))
  )
  result <- prop_table_afun(
    x = factor(c("a", "b", "a"), levels = c("b", "a")),
    .spl_context = fake_spl_context,
    formats = formats
  )
  expect_snapshot(result)

  fake_spl_context <- data.frame(
    cur_col_split_val = I(list(c(
      x = "a",
      b = "c",
      cum_percent = "cum_percent"
    )))
  )
  result <- prop_table_afun(
    x = factor(c("a", "b", "a"), levels = c("b", "a")),
    .spl_context = fake_spl_context,
    formats = formats
  )
  expect_snapshot(result)

  fake_spl_context <- data.frame(
    cur_col_split_val = I(list(c(x = "a", b = "c", bla = "foo")))
  )
  expect_error(
    prop_table_afun(
      x = factor(c("a", "b", "a"), levels = c("b", "a")),
      .spl_context = fake_spl_context,
      formats = formats
    ),
    "unexpected"
  )
})
