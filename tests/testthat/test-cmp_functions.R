test_that("cmp_split_fun works as expected", {
  result <- basic_table(round_type = "sas") %>%
    split_cols_by("ID", split_fun = cmp_split_fun) |>
    build_table(formatters::DM)
  expect_snapshot(result)
})

test_that("cmp_cfun works correctly for expected column", {
  fake_spl_context <- data.frame(
    cur_col_split_val = I(list(c(ARM = "A: Drug X", expected = "expected")))
  )
  df <- data.frame(exp = c(TRUE, TRUE, FALSE))
  result <- cmp_cfun(
    df,
    labelstr = "Visit 1",
    .spl_context = fake_spl_context,
    variables = list(expected = "exp", received = "rec", missing = "mis"),
    formats = list()
  )
  expect_snapshot(result)
})

test_that("cmp_cfun works correctly for received column", {
  fake_spl_context <- data.frame(
    cur_col_split_val = I(list(c(ARM = "A: Drug X", received = "received")))
  )
  df <- data.frame(
    exp = c(TRUE, TRUE, FALSE),
    rec = c(TRUE, FALSE, FALSE)
  )
  result <- cmp_cfun(
    df,
    labelstr = "Visit 1",
    .spl_context = fake_spl_context,
    variables = list(expected = "exp", received = "rec", missing = "mis"),
    formats = list(count_percent = jjcsformat_count_fraction)
  )
  expect_snapshot(result)
})

test_that("cmp_cfun works correctly for missing column", {
  fake_spl_context <- data.frame(
    cur_col_split_val = I(list(c(ARM = "A: Drug X", missing = "missing")))
  )
  df <- data.frame(
    exp = c(TRUE, TRUE, FALSE),
    rec = c(TRUE, FALSE, FALSE),
    mis = c(FALSE, TRUE, FALSE)
  )
  result <- cmp_cfun(
    df,
    labelstr = "Visit 1",
    .spl_context = fake_spl_context,
    variables = list(expected = "exp", received = "rec", missing = "mis"),
    formats = list(count_percent = jjcsformat_count_fraction)
  )
  expect_snapshot(result)
})
