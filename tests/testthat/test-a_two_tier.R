n <- 36
set.seed(1)
data <- data.frame(
  trt = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
  eostt = factor(sample(c("COMPLETED", "DISCONTINUED", "ONGOING"), n, replace = TRUE)),
  dcsreas = factor(sample(c("ADVERSE EVENT", "LACK OF EFFICACY", "PHYSICIAN DECISION"), n, replace = TRUE))
)

# Start of tests ----

test_that("a_two_tier works silently for ex_adsl data with simple_analysis", {
  lyt <- basic_table() |>
    split_cols_by("ARM") |>
    split_rows_by("EOSSTT", child_labels = "hidden") |>
    analyze("EOSSTT",
      afun = a_two_tier,
      extra_args = list(
        grp_fun = simple_analysis,
        detail_fun = simple_analysis,
        inner_var = "DCSREAS",
        drill_down_levs = "DISCONTINUED"
      )
    )

  expect_silent(build_table(lyt, ex_adsl))
})

test_that("a_two_tier works in a table layout as expected", {
  lyt <- basic_table() |>
    split_cols_by("trt") |>
    split_rows_by("eostt", child_labels = "hidden") |>
    analyze("eostt",
      afun = a_two_tier,
      extra_args = list(
        grp_fun = simple_analysis,
        detail_fun = simple_analysis,
        inner_var = "dcsreas",
        drill_down_levs = "DISCONTINUED"
      )
    )
  res <- expect_silent(build_table(lyt, data))
  res_act <- matrix_form(res)$string
  res_exp <- structure(
    c(
      "", "COMPLETED", "DISCONTINUED", "ADVERSE EVENT", "LACK OF EFFICACY", "PHYSICIAN DECISION", "ONGOING",
      "A", "3", "9", "3", "4", "2", "3",
      "B", "2", "5", "2", "0", "3", "5",
      "C", "3", "3", "0", "2", "1", "3"
    ),
    dim = c(7, 4)
  )
  expect_identical(res_act, res_exp)
})

test_that("a_two_tier produces the expected table layout when a level has no observations", {
  lyt <- basic_table() |>
    split_cols_by("trt") |>
    split_rows_by("eostt", child_labels = "hidden") |>
    analyze("eostt",
      afun = a_two_tier,
      extra_args = list(
        grp_fun = simple_analysis,
        detail_fun = simple_analysis,
        inner_var = "dcsreas",
        drill_down_levs = "DISCONTINUED"
      )
    )

  data_subset <- subset(data, dcsreas != "ADVERSE EVENT")

  res <- expect_silent(build_table(lyt, data_subset))
  res_act <- matrix_form(res)$string
  res_exp <- structure(
    c(
      "", "COMPLETED", "DISCONTINUED", "LACK OF EFFICACY", "PHYSICIAN DECISION", "ONGOING",
      "A", "3", "6", "4", "2", "2",
      "B", "1", "3", "0", "3", "4",
      "C", "1", "3", "2", "1", "2"
    ),
    dim = c(6, 4)
  )
  expect_identical(res_act, res_exp)
})

test_that("a_two_tier produces the expected table layout when a level has no observations (use_all_levels)", {
  lyt <- basic_table() |>
    split_cols_by("trt") |>
    split_rows_by("eostt", child_labels = "hidden") |>
    analyze("eostt",
      afun = a_two_tier,
      extra_args = list(
        grp_fun = simple_analysis,
        detail_fun = simple_analysis,
        inner_var = "dcsreas",
        drill_down_levs = "DISCONTINUED",
        use_all_levels = TRUE
      )
    )

  data_subset <- subset(data, dcsreas != "ADVERSE EVENT")

  res <- expect_silent(build_table(lyt, data_subset))
  res_act <- matrix_form(res)$string
  res_exp <- structure(
    c(
      "", "COMPLETED", "DISCONTINUED", "ADVERSE EVENT", "LACK OF EFFICACY", "PHYSICIAN DECISION", "ONGOING",
      "A", "3", "6", "0", "4", "2", "2",
      "B", "1", "3", "0", "0", "3", "4",
      "C", "1", "3", "0", "2", "1", "2"
    ),
    dim = c(7, 4)
  )
  expect_identical(res_act, res_exp)
})

test_that("a_two_tier produces the expected table layout when there are no observations for any level", {
  lyt <- basic_table() |>
    split_cols_by("trt") |>
    split_rows_by("eostt", child_labels = "hidden") |>
    analyze("eostt",
      afun = a_two_tier,
      extra_args = list(
        grp_fun = simple_analysis,
        detail_fun = simple_analysis,
        inner_var = "dcsreas",
        drill_down_levs = "DISCONTINUED"
      )
    )

  data_subset <- subset(data, eostt != "DISCONTINUED")

  res <- expect_silent(build_table(lyt, data_subset))
  res_act <- matrix_form(res)$string
  res_exp <- structure(
    c(
      "", "COMPLETED", "DISCONTINUED", "ONGOING",
      "A", "3", "0", "3",
      "B", "2", "0", "5",
      "C", "3", "0", "3"
    ),
    dim = c(4, 4)
  )
  expect_identical(res_act, res_exp)
})

test_that("a_two_tier produces the expected table layout - no observations at any level (use_all_levels)", {
  lyt <- basic_table() |>
    split_cols_by("trt") |>
    split_rows_by("eostt", child_labels = "hidden") |>
    analyze("eostt",
      afun = a_two_tier,
      extra_args = list(
        grp_fun = simple_analysis,
        detail_fun = simple_analysis,
        inner_var = "dcsreas",
        drill_down_levs = "DISCONTINUED",
        use_all_levels = TRUE
      )
    )

  data_subset <- subset(data, eostt != "DISCONTINUED")

  res <- expect_silent(build_table(lyt, data_subset))
  res_act <- matrix_form(res)$string
  res_exp <- structure(
    c(
      "", "COMPLETED", "DISCONTINUED", "ONGOING",
      "A", "3", "0", "3",
      "B", "2", "0", "5",
      "C", "3", "0", "3"
    ),
    dim = c(4, 4)
  )
  expect_identical(res_act, res_exp)
})

test_that("a_two_tier produces the expected table layout when there is no data at all - only levels", {
  lyt <- basic_table() |>
    split_cols_by("trt") |>
    split_rows_by("eostt", child_labels = "hidden") |>
    analyze("eostt",
      afun = a_two_tier,
      extra_args = list(
        grp_fun = simple_analysis,
        detail_fun = simple_analysis,
        inner_var = "dcsreas",
        drill_down_levs = "DISCONTINUED"
      )
    )

  res <- expect_silent(build_table(lyt, data[0, ]))
  res_act <- matrix_form(res)$string
  res_exp <- structure(
    c(
      "", "COMPLETED", "DISCONTINUED", "ONGOING",
      "A", "0", "0", "0",
      "B", "0", "0", "0",
      "C", "0", "0", "0"
    ),
    dim = c(4, 4)
  )
  expect_identical(res_act, res_exp)
})

test_that("a_two_tier produces the expected table layout when there is no data at all - only levels (use_all_levels)", {
  lyt <- basic_table() |>
    split_cols_by("trt") |>
    split_rows_by("eostt", child_labels = "hidden") |>
    analyze("eostt",
      afun = a_two_tier,
      extra_args = list(
        grp_fun = simple_analysis,
        detail_fun = simple_analysis,
        inner_var = "dcsreas",
        drill_down_levs = "DISCONTINUED",
        use_all_levels = TRUE
      )
    )

  res <- expect_silent(build_table(lyt, data[0, ]))
  res_act <- matrix_form(res)$string
  res_exp <- structure(
    c(
      "", "COMPLETED", "DISCONTINUED", "ONGOING",
      "A", "0", "0", "0",
      "B", "0", "0", "0",
      "C", "0", "0", "0"
    ),
    dim = c(4, 4)
  )
  expect_identical(res_act, res_exp)
})
