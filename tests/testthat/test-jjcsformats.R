values <- c(5.123456, 7.891112)

format_value <- formatters::format_value

## need to update tests when formatters would allow to pass na_str onto formatting functions
## now the NA formatting is purely handled by the formatting function and for jjcsformat_xx the
## default handling of NA is NE

NA_str_jjcs <- "NE"


test_that("jjjcs formats work", {
  ## core formatter tests for format strings
  expect_snapshot(cran = TRUE, {
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
  expect_snapshot(cran = TRUE, {
    format_value(NA, "xx.", na_str = "-")
    format_value(NA, "xx", na_str = "-")
    format_value(c(1, NA), "xx")
  })

  ## trailing 0s are correct
  expect_snapshot(cran = TRUE, {
    format_value(0, "xx.")
    format_value(0, "xx.x")
    format_value(0, "xx.xx")
    format_value(0, "xx.xxx")
    format_value(0, "xx.xxxx")
  })

  ### untill formatters::format_value isn't adjusted to pass na_str into a formatting function
  ### set the expectation to NA - NA
  expect_snapshot(cran = TRUE, {
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


test_that("round_type support works", {
  val <- 7.05 ## differs for xx.x between round types
  expect_equal(
    format_value(val, format = jjcsformat_xx("xx.x"), round_type = "sas"),
    format_value(val, format = "xx.x", round_type = "sas")
  ) # nolint start
  expect_false(format_value(val, format = jjcsformat_xx("xx.x"), round_type = "sas") ==
    format_value(val, format = "xx.x", round_type = "iec"))
  # nolint end
  val2 <- c(5, 0.9945)

  expect_equal(
    format_value(val2, format = "xx (xx.x%)", round_type = "sas"),
    jjcsformat_count_fraction(val2, round_type = "sas")
  )
  expect_equal(
    format_value(val2, format = "xx (xx.x%)", round_type = "iec"),
    jjcsformat_count_fraction(val2, round_type = "iec")
  )
  # nolint start
  expect_false(jjcsformat_count_fraction(val2, round_type = "sas") ==
    format_value(val2, "xx (xx.x%)", round_type = "iec"))
  # nolint end
  val3 <- c(5, 10, 0.9945)

  ## these differ for now :(:(:(:( xx/xx (xx.x%)  vs xx / xx (xx.x%)
  add_spcs_fmt <- function(str) gsub("/", " / ", str, fixed = TRUE)
  expect_equal(
    format_value(val3, format = "xx / xx (xx.x%)", round_type = "sas"),
    add_spcs_fmt(jjcsformat_count_denom_fraction(val3, round_type = "sas"))
  )
  expect_equal(
    format_value(val3, format = "xx / xx (xx.x%)", round_type = "iec"),
    add_spcs_fmt(jjcsformat_count_denom_fraction(val3, round_type = "iec"))
  )
  # nolint start
  expect_false(add_spcs_fmt(jjcsformat_count_denom_fraction(val3, round_type = "sas")) ==
    format_value(val3, "xx / xx (xx.x%)", round_type = "iec"))

  expect_false(jjcsformat_fraction_count_denom(val3, round_type = "sas") ==
    jjcsformat_fraction_count_denom(val3, round_type = "iec"))
  # nolint end
})

test_that("jjcsformat_range_fct is formatting ranges as expected", {
  my_range_format <- jjcsformat_range_fct("xx.xx")
  my_range_format2 <- jjcsformat_range_fct("xx.xx", censor_char = "*")
  expect_snapshot(cran = TRUE, {
    my_range_format(c(0.35235, 99.2342, 1, 0))
    my_range_format(c(0.35235, 99.2342, 0, 1))
    my_range_format(c(0.35235, 99.2342, 0, 0))
    my_range_format(c(0.35235, 99.2342, 1, 1))
    my_range_format2(c(0.35235, 99.2342, 0, 1))
  })
})

test_that("jjcsformat_pval_fct works", {
  expect_snapshot(cran = TRUE, {
    jjcsformat_pval_fct(0.005)(0.0048)
    jjcsformat_pval_fct(0.005)(0.00499)
    jjcsformat_pval_fct(0.005)(0.000499)
    jjcsformat_pval_fct(0)(0.0048)
    jjcsformat_pval_fct(0.05)(0.0048)
    jjcsformat_pval_fct(0.005)(0.0051)
    jjcsformat_pval_fct(0)(0.00001)
    jjcsformat_pval_fct(0)(0.0009999999)
    jjcsformat_pval_fct(0)(0.001)
    jjcsformat_pval_fct(0)(0.9999)
    jjcsformat_pval_fct(0)(0.999)
    jjcsformat_pval_fct(0)(0.9990000001)
    jjcsformat_pval_fct(0)(NA_real_, na_str = "ne")
    jjcsformat_pval_fct(0.0005)(NA_real_, na_str = "ne")
    jjcsformat_pval_fct(0.005)(0.004999999)
    jjcsformat_pval_fct(0.005)(0.0049999999)
    jjcsformat_pval_fct(0.005)(0.00499999999)
  })
})

test_that("some special cases for jjcsformat_pval_fct", {
  expect_identical(
    format_value(NA_real_, format = jjcsformat_pval_fct(0), na_str = "NE"),
    "NE"
  )
  expect_identical(
    format_value(NA_real_, format = jjcsformat_pval_fct(0.0005), na_str = "NE"),
    "NE"
  )
  expect_error(
    format_value(0.00000123, format = jjcsformat_pval_fct(0.0005), na_str = "NE"),
    "jjcsformat_pval_fct: argument alpha should be 0 or at least 0.001."
  )
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

# tests for format_sigfig_j start here ----
test_that("format_sigfig_j case 1", {
  x <- 0.35769
  x2 <- x * c(1, 10, 100, 1000, 10000)
  x2

  expect_identical(
    signif(x2, digits = 3),
    modified_signif_j(x2, digits = 3)
  )

  expect_identical(
    signif(x2, digits = 3)[x2 < 1000],
    modified_signif_j(x2, digits = 3, whole_integer = TRUE)[x2 < 1000]
  )

  expect_any_difference(
    signif(x2, digits = 3)[x2 >= 1000],
    modified_signif_j(x2, digits = 3, whole_integer = TRUE)[x2 >= 1000]
  )

  expect_identical(
    modified_signif_j(x2, digits = 3, whole_integer = TRUE)[x2 >= 1000],
    round(x2[x2 >= 1000], 0)
  )


  fmt_3sf_j <- format_sigfig_j(3, whole_integer = TRUE)
  fmt_3sf <- format_sigfig(3)

  expect_identical(
    fmt_3sf_j(x2)[x2 < 1000],
    fmt_3sf(x2)[x2 < 1000]
  )

  expect_any_difference(
    fmt_3sf_j(x2)[x2 >= 1000],
    fmt_3sf(x2)[x2 >= 1000]
  )

  expect_identical(
    fmt_3sf_j(x2)[x2 >= 1000],
    as.character(round(x2[x2 >= 1000], 0))
  )
})


test_that("format_sigfig_j case 2 - focus on rounding", {
  x <- 0.1645
  x2 <- x * c(1, 10, 100, 1000, 10000)
  x2

  fmt_3sf_j <- format_sigfig_j(3, whole_integer = TRUE)
  fmt_3sf <- format_sigfig(3)

  expect_identical(
    fmt_3sf_j(x2)[x2 < 1000],
    fmt_3sf(x2)[x2 < 1000]
  )

  expect_any_difference(
    fmt_3sf_j(x2)[x2 >= 1000],
    fmt_3sf(x2)[x2 >= 1000]
  )

  expect_identical(
    fmt_3sf_j(x2)[x2 >= 1000],
    as.character(round(x2[x2 >= 1000], 0))
  )

  expect_any_difference(
    fmt_3sf_j(x2, round_type = "sas")[x2 < 1000],
    fmt_3sf(x2)[x2 < 1000]
  )

  expect_equal(
    fmt_3sf_j(x2, round_type = "sas")[x2 < 1000],
    fmt_3sf(x2 + 0.0001)[x2 < 1000]
  )

  expect_identical(
    fmt_3sf_j(x2)[x2 >= 1000],
    as.character(round(x2[x2 >= 1000], 0))
  )

  expect_any_difference(
    fmt_3sf_j(1645.5, round_type = "sas"),
    fmt_3sf(1645.5)
  )

  expect_any_difference(
    fmt_3sf_j(1644.5, round_type = "sas"),
    fmt_3sf_j(1644.5, round_type = "iec")
  )

  expect_any_difference(
    round(1644.5, 0),
    roundSAS(1644.5, 0)
  )

  expect_identical(
    fmt_3sf_j(1644.5, round_type = "sas"),
    as.character(roundSAS(1644.5, 0))
  )
})

test_that("format_sigfig_j case 3 - focus on values almost zero", {
  x <- 1e-10

  fmt_3sf_j <- format_sigfig_j(3)
  fmt_3sf_j_2 <- format_sigfig_j(3, zero_threshold = 0)
  fmt_3sf <- format_sigfig(3)

  expect_identical(
    fmt_3sf_j_2(x),
    fmt_3sf(x)
  )

  expect_any_difference(
    fmt_3sf_j(x),
    fmt_3sf(x)
  )

  expect_identical(
    fmt_3sf_j(x),
    "0"
  )

  expect_identical(
    fmt_3sf_j_2(x),
    "0.000000000100"
  )
})

test_that("format_sigfig_j case 4 - focus on trailing zeros", {
  cur_scipen_opt <- getOption("scipen")
  options(scipen = 999)

  xx <- 10^seq(2, by = -1, length.out = 7)
  x <- 4 * xx

  x_target_t0 <- c("400", "40.0", "4.00", "0.400", "0.0400", "0.00400", "0.000400")
  x_target_nt0 <- c("400", "40", "4", "0.4", "0.04", "0.004", "0.0004")

  fmt_3sf_j <- format_sigfig_j(3, drop0trailing = FALSE)
  fmt_3sf_j_nt0 <- format_sigfig_j(3, drop0trailing = TRUE)

  expect_identical(
    fmt_3sf_j(x),
    x_target_t0
  )

  expect_identical(
    fmt_3sf_j_nt0(x),
    x_target_nt0
  )

  options("scipen" = cur_scipen_opt)
})

test_that("format_sigfig_j used in rtables framework as format", {
  adsl <- ex_adsl
  adsl$BMRKR1[adsl$ARMCD == "ARM A"] <- 1.845

  lyt <- basic_table() |>
    split_cols_by("ARMCD") |>
    analyze("BMRKR1",
      afun = a_summary,
      extra_args = list(
        .stats = c("n", "mean_sd", "range"),
        .formats = c(
          "mean_sd" = format_sigfig_j(3, format = "xx (xx)"),
          "range" = format_sigfig_j(3, format = "xx, xx")
        )
      )
    )

  rslt <- build_table(lyt, adsl, round_type = "sas")
  rslt
  expect_snapshot(cran = TRUE, rslt)

  rslt2 <- build_table(lyt, adsl, round_type = "iec")
  rslt2
  expect_snapshot(cran = TRUE, rslt)
})

test_that("format_sigfig_j used in rtables framework as format w/wout trailing zeros", {
  adsl <- ex_adsl
  adsl$BMRKR1[adsl$ARMCD == "ARM A"] <- 0.0004
  adsl$BMRKR1[adsl$ARMCD == "ARM B"] <- 4

  lyt <- basic_table() |>
    split_cols_by("ARMCD") |>
    analyze("BMRKR1",
      afun = a_summary,
      extra_args = list(
        .stats = c("n", "mean_sd", "range"),
        .formats = c(
          "mean_sd" = format_sigfig_j(3, format = "xx (xx)", drop0trailing = TRUE),
          "range" = format_sigfig_j(3, format = "xx, xx", drop0trailing = FALSE)
        )
      )
    )

  rslt <- build_table(lyt, adsl)
  rslt
  expect_snapshot(cran = TRUE, rslt)
})

test_that("explicit modified_signif_j tests - assertions", {
  expect_error(
    modified_signif_j(c(0.1, 1e-9), digits = 3, zero_threshold = 1e-2),
    "Assertion on 'zero_threshold' failed: Element 1 is not <= 0.001."
  )

  expect_error(
    modified_signif_j(c(0.1, 1e-9), digits = 3, zero_threshold = -2),
    "Assertion on 'zero_threshold' failed: Element 1 is not >= 0."
  )

  expect_error(
    modified_signif_j(c(0.1, 1e-9), digits = 3, zero_threshold = TRUE),
    "Assertion on 'zero_threshold' failed: Must be of type 'number'"
  )
})

test_that("explicit modified_signif_j tests - zero_threshold", {
  expect_identical(
    modified_signif_j(c(0.1, 1e-9), digits = 3, zero_threshold = 1e-5),
    c(0.1, 0)
  )
})

test_that("explicit modified_signif_j tests - same as signif", {
  expect_identical(
    modified_signif_j(c(0.1, 1e-9), digits = 3, zero_threshold = 0),
    signif(c(0.1, 1e-9), digits = 3)
  )
})

test_that("explicit modified_signif_j tests - round_type", {
  expect_identical(
    modified_signif_j(c(0.1, 1e-9, 1256.0, 1256.5, 1256.3, 1254.5, 1256.5),
      digits = 3,
      zero_threshold = 0,
      whole_integer = TRUE,
      round_type = "sas"
    ),
    c(0.1, 1e-9, 1256, 1257, 1256, 1255, 1257)
  )

  expect_identical(
    modified_signif_j(c(0.1, 1e-9, 1256.0, 1254.5, 1256.3, 1254.5, 1256.5),
      digits = 3,
      zero_threshold = 0,
      whole_integer = TRUE,
      round_type = "iec"
    ),
    c(0.1, 1e-9, 1256, 1254, 1256, 1254, 1256)
  )

  expect_identical(
    modified_signif_j(c(0.1, 1e-9, 1256.0, 1256.5, 1256.3),
      digits = 3,
      zero_threshold = 0,
      whole_integer = FALSE,
      round_type = "sas"
    ),
    c(0.1, 1e-9, 1260, 1260, 1260)
  )

  expect_identical(
    modified_signif_j(c(0.1, 1e-9, 1256.0, 1256.5, 1256.3),
      digits = 3,
      zero_threshold = 0,
      whole_integer = FALSE,
      round_type = "iec"
    ),
    signif(c(0.1, 1e-9, 1256.0, 1256.5, 1256.3), digits = 3)
  )
})
