values <- c(5.123456, 7.891112)

format_value <- formatters::format_value

## need to update tests when formatters would allow to pass na_str onto formatting functions
## now the NA formatting is purely handled by the formatting function and for jjcsformat_xx the
## default handling of NA is NE

NA_str_jjcs <- "NE"


test_that("jjjcs formats work", {
  ## core formatter tests for format strings
  expect_snapshot({
    format_value(values[1], format = jjcsformat_xx("xx"))
    format_value(values[1], format = jjcsformat_xx("xx."))
    format_value(values[1], format = jjcsformat_xx("xx.x"))
    format_value(values[1], format = jjcsformat_xx("xx.xx"))
    format_value(values[1], format = jjcsformat_xx("xx.xxx"))
    format_value(values[1], format = jjcsformat_xx("xx.xxxx"))
    format_value(values, format = jjcsformat_xx("(xx, xx)"))
    format_value(values, format = jjcsformat_xx("(xx., xx.)"))
    format_value(values, format = jjcsformat_xx("(xx.x, xx.x)"))
    format_value(values, format = jjcsformat_xx("(xx.xx, xx.xx)"))
    format_value(values, format = jjcsformat_xx("(xx.xxx, xx.xxx)"))
    format_value(values, format = jjcsformat_xx("(xx.xxxx, xx.xxxx)"))
    format_value(values, format = jjcsformat_xx("xx - xx"))
    format_value(values, format = jjcsformat_xx("xx.x - xx.x"))
    format_value(values, format = jjcsformat_xx("xx.xx - xx.xx"))
    format_value(values, format = jjcsformat_xx("xx (xx)"))
    format_value(values, format = jjcsformat_xx("xx (xx.)"))
    format_value(values, format = jjcsformat_xx("xx (xx.x)"))
    format_value(values, format = jjcsformat_xx("xx (xx.xx)"))
    format_value(values, format = jjcsformat_xx("xx. (xx.)"))
    format_value(values, format = jjcsformat_xx("xx.x (xx.x)"))
    format_value(values, format = jjcsformat_xx("xx.xx (xx.xx)"))
    format_value(values, format = jjcsformat_xx("xx.x, xx.x"))
    format_value(values, format = jjcsformat_xx("xx.x to xx.x"))
    format_value(c(values, 10.1235), format = jjcsformat_xx("xx. (xx. - xx.)"))
    format_value(
      c(values, 10.1235),
      format = jjcsformat_xx("xx.x (xx.x - xx.x)")
    )
    format_value(
      c(values, 10.1235),
      format = jjcsformat_xx("xx.xx (xx.xx - xx.xx)")
    )
    format_value(
      c(values, 10.1235),
      format = jjcsformat_xx("xx.xxx (xx.xxx - xx.xxx)")
    )
  })

  ## handling NAs
  expect_snapshot({
    format_value(NA, "xx.", na_str = "-")
    format_value(NA, "xx", na_str = "-")
    format_value(c(1, NA), "xx")
  })

  ## trailing 0s are correct
  expect_snapshot({
    format_value(0, "xx.")
    format_value(0, "xx.x")
    format_value(0, "xx.xx")
    format_value(0, "xx.xxx")
    format_value(0, "xx.xxxx")
  })

  ### untill formatters::format_value isn't adjusted to pass na_str into a formatting function
  ### set the expectation to NA - NA
  expect_snapshot({
    format_value(
      c(NA, NA),
      format = jjcsformat_xx("xx.x - xx.x"),
      na_str = c("hi", "lo")
    )

    ### untill formatters::format_value isn't adjusted to pass na_str into a formatting function
    ### set the expectation to NA - 5.2
    format_value(
      c(NA, 5.2),
      format = jjcsformat_xx("xx.x - xx.x"),
      na_str = "what"
    )

    ### untill formatters::format_value isn't adjusted to pass na_str into a formatting function
    ### set the expectation to NA - 5.2
    format_value(
      c(NA, 5.2),
      format = jjcsformat_xx("xx.x - xx.x"),
      na_str = c("hi", "lo")
    )

    # for all values NA and na_str is of length 1, also formatting functions would give the same result
    format_value(
      c(NA, NA),
      format = jjcsformat_xx("xx.x - xx.x"),
      na_str = "what"
    )

    format_value(NA, format = jjcsformat_xx("xx.x"), na_str = character())

    format_value(NA, format = jjcsformat_xx("xx.x"), na_str = NA_character_)
  })
})

## round type works


test_that("roundmethod support works", {
  val <- 7.05 ## differs for xx.x between round types
  expect_equal(
    format_value(val, format = jjcsformat_xx("xx.x")),
    format_value(val, format = "xx.x", round_type = "sas")
  ) # nolint start
  expect_false(format_value(val, format = jjcsformat_xx("xx.x")) ==
    format_value(val, format = "xx.x", round_type = "iec"))
  # nolint end
  val2 <- c(5, 0.9945)

  expect_equal(
    format_value(val2, format = "xx (xx.x%)", round_type = "sas"),
    jjcsformat_count_fraction(val2, roundmethod = "sas")
  )
  expect_equal(
    format_value(val2, format = "xx (xx.x%)", round_type = "iec"),
    jjcsformat_count_fraction(val2, roundmethod = "iec")
  )
  # nolint start
  expect_false(jjcsformat_count_fraction(val2, roundmethod = "sas") ==
    format_value(val2, "xx (xx.x%)", round_type = "iec"))
  # nolint end
  val3 <- c(5, 10, 0.9945)

  ## these differ for now :(:(:(:( xx/xx (xx.x%)  vs xx / xx (xx.x%)
  add_spcs_fmt <- function(str) gsub("/", " / ", str, fixed = TRUE)
  expect_equal(
    format_value(val3, format = "xx / xx (xx.x%)", round_type = "sas"),
    add_spcs_fmt(jjcsformat_count_denom_fraction(val3, roundmethod = "sas"))
  )
  expect_equal(
    format_value(val3, format = "xx / xx (xx.x%)", round_type = "iec"),
    add_spcs_fmt(jjcsformat_count_denom_fraction(val3, roundmethod = "iec"))
  )
  # nolint start
  expect_false(add_spcs_fmt(jjcsformat_count_denom_fraction(val3, roundmethod = "sas")) ==
    format_value(val3, "xx / xx (xx.x%)", round_type = "iec"))

  expect_false(jjcsformat_fraction_count_denom(val3, roundmethod = "sas") ==
    jjcsformat_fraction_count_denom(val3, roundmethod = "iec"))
  # nolint end
})

test_that("jjcsformat_range_fct is formatting ranges as expected", {
  my_range_format <- jjcsformat_range_fct("xx.xx")
  expect_snapshot({
    my_range_format(c(0.35235, 99.2342, 1, 0))
    my_range_format(c(0.35235, 99.2342, 0, 1))
    my_range_format(c(0.35235, 99.2342, 0, 0))
    my_range_format(c(0.35235, 99.2342, 1, 1))
  })
})

test_that("jjcsformat_pval_fct works", {
  expect_snapshot({
    jjcsformat_pval_fct(0.005)(0.0048)
    jjcsformat_pval_fct(0.005)(0.00499)
    jjcsformat_pval_fct(0)(0.0048)
    jjcsformat_pval_fct(0.05)(0.0048)
    jjcsformat_pval_fct(0.005)(0.0051)
    jjcsformat_pval_fct(0)(0.00001)
    jjcsformat_pval_fct(0)(0.0009999999)
    jjcsformat_pval_fct(0)(0.001)
    jjcsformat_pval_fct(0)(0.9999)
    jjcsformat_pval_fct(0)(0.999)
    jjcsformat_pval_fct(0)(0.9990000001)
  })
})

test_that("jjcsformat_xx works also for empty cells", {
  expect_silent(in_rows(
    .list = list(
      or_ci = structure(list(), label = "Odds Ratio (95% CI)"),
      pval = NULL
    ),
    .formats = list(
      or_ci = jjcsformat_xx("xx.xx (xx.xx - xx.xx)"),
      pval = jjcsformat_pval_fct(0)
    ),
    .labels = list(
      or_ci = "Odds Ratio (95% CI)",
      pval = "p-value"
    )
  ))
})

test_that("jjcsformat_xx works also for cells with 0 length vectors", {
  expect_silent(in_rows(
    .list = list(
      or_ci = structure(numeric(), label = "Odds Ratio (95% CI)"),
      pval = NULL
    ),
    .formats = list(
      or_ci = jjcsformat_xx("xx.xx (xx.xx - xx.xx)"),
      pval = jjcsformat_pval_fct(0)
    ),
    .labels = list(
      or_ci = "Odds Ratio (95% CI)",
      pval = "p-value"
    )
  ))
})
