values <- c(5.123456, 7.891112)

cdf <- c(5, 2000, 5 / 2000)
cf <- c(5, 5 / 2000)

format_value <- formatters::format_value

NA_str_jjcs <- "NE"

test_that("jjcs_num_formats works", {
  # jjcs_num_formats spec tests
  expect_snapshot({
    jjcs_num_formats(d = 0)$spec
    jjcs_num_formats(d = 1)$spec
    jjcs_num_formats(d = 8, cap = 2)$spec
    jjcs_num_formats(NA)$spec
  })

  ## errors

  expect_error(
    jjcs_num_formats(d = 0.1)$spec,
    "Assertion on 'd' failed: Must be of type 'count', not 'double'"
  )
  expect_error(
    jjcs_num_formats(d = "asis")$spec,
    "Assertion on 'd' failed: Must be of type 'count', not 'character'"
  )

  # jjcsformat_xx_SAS/R format tests
  expect_snapshot({
    format_value(values, format = jjcsformat_xx("xx.x (xx.xx)"), round_type = "sas")
    format_value(values, format = jjcsformat_xx("xx.x (xx.xx)"), round_type = "iec")
    format_value(c(5.05, values[2]), format = jjcsformat_xx("xx.x (xx.xx)"), round_type = "sas")
    format_value(c(5.05, values[2]), format = jjcsformat_xx("xx.x (xx.xx)"), round_type = "iec")
    format_value(c(5.15, values[2]), format = jjcsformat_xx("xx.x (xx.xx)"), round_type = "iec")
    format_value(c(5.15, values[2]), format = "xx.x (xx.x)")
    format_value(c(4.15, values[2]), format = jjcsformat_xx("xx.x (xx.xx)"), round_type = "sas")
    format_value(c(4.15, values[2]), format = jjcsformat_xx("xx.x (xx.xx)"), round_type = "iec")
    format_value(c(4.15, values[2]), format = "xx.x (xx.x)")
    format_value(c(4.15, values[2]), format = jjcsformat_xx("xx.x (xx.x)"), round_type = "sas")
    format_value(c(3.15, values[2]), format = "xx.x (xx.x)")
    format_value(c(3.15, values[2]), format = jjcsformat_xx("xx.x (xx.x)"), round_type = "sas")
    format_value(c(3.15, values[2]), format = jjcsformat_xx("xx.x (xx.x)"), round_type = "iec")
  })

  # jjcsformat_xx_SAS format tests
  expect_snapshot({
    format_value(values, format = jjcsformat_xx("xx / xx"), round_type = "sas")
    format_value(values, format = jjcsformat_xx("xx. / xx."), round_type = "sas")
    format_value(values, format = jjcsformat_xx("xx.x / xx.x"), round_type = "sas")
    format_value(values, format = jjcsformat_xx("xx.xx / xx.xx"), round_type = "sas")
    format_value(values, format = jjcsformat_xx("xx.xxx / xx.xxx"), round_type = "sas")
    format_value(values, format = jjcsformat_xx("(xx, xx)"), round_type = "sas")
    format_value(values, format = jjcsformat_xx("(xx., xx.)"), round_type = "sas")
    format_value(values, format = jjcsformat_xx("(xx.x, xx.x)"), round_type = "sas")
    format_value(values, format = jjcsformat_xx("(xx.xx, xx.xx)"), round_type = "sas")
    format_value(values, format = jjcsformat_xx("(xx.xxx, xx.xxx)"), round_type = "sas")
    format_value(values, format = jjcsformat_xx("(xx.xxxx, xx.xxxx)"), round_type = "sas")
    format_value(values, format = jjcsformat_xx("xx - xx"), round_type = "sas")
    format_value(values, format = jjcsformat_xx("xx.x - xx.x"), round_type = "sas")
    format_value(values, format = jjcsformat_xx("xx.xx - xx.xx"), round_type = "sas")
    format_value(values, format = jjcsformat_xx("xx (xx)"), round_type = "sas")
    format_value(values, format = jjcsformat_xx("xx (xx.)"), round_type = "sas")
    format_value(values, format = jjcsformat_xx("xx (xx.x)"), round_type = "sas")
    format_value(values, format = jjcsformat_xx("xx (xx.xx)"), round_type = "sas")
    format_value(values, format = jjcsformat_xx("xx. (xx.)"), round_type = "sas")
    format_value(values, format = jjcsformat_xx("xx.x (xx.x)"), round_type = "sas")
    format_value(values, format = jjcsformat_xx("xx.xx (xx.xx)"), round_type = "sas")
    format_value(values, format = jjcsformat_xx("xx.x, xx.x"), round_type = "sas")
    format_value(values, format = jjcsformat_xx("xx.x to xx.x"), round_type = "sas")
    format_value(
      c(values, 10.1235),
      format = jjcsformat_xx("xx. (xx. - xx.)"),
      round_type = "sas"
    )
    format_value(
      c(values, 10.1235),
      format = jjcsformat_xx("xx.x (xx.x - xx.x)"),
      round_type = "sas"
    )
    format_value(
      c(values, 10.1235),
      format = jjcsformat_xx("xx.xx (xx.xx - xx.xx)"),
      round_type = "sas"
    )
    format_value(
      c(values, 10.1235),
      format = jjcsformat_xx("xx.xxx (xx.xxx - xx.xxx)"),
      round_type = "sas"
    )
    format_value(NULL, jjcsformat_xx("xx"), round_type = "sas")
    format_value(c(500), jjcsformat_xx("N=xx"), round_type = "sas")
    format_value(c(500), jjcsformat_xx("(N=xx)"), round_type = "sas")
  })

  ## errors

  expect_error(
    format_value(5.1, jjcsformat_xx("abcd")),
    "input str must contain xx"
  )
  expect_error(
    format_value(5.1, jjcsformat_xx("xx + xx")),
    "jjcsformat_xx must contain same number of xx as the number of stats"
  )

  expect_error(
    format_value(c(5.1, 2, 3), jjcsformat_xx("xx + xx")),
    "jjcsformat_xx must contain same number of xx as the number of stats"
  )

  ## trailing 0s are correct
  expect_snapshot({
    format_value(0, jjcsformat_xx("xx."), round_type = "sas")
    format_value(0, jjcsformat_xx("xx.x"), round_type = "sas")
    format_value(0, jjcsformat_xx("xx.xx"), round_type = "sas")
    format_value(0, jjcsformat_xx("xx.xxx"), round_type = "sas")
    format_value(0, jjcsformat_xx("xx.xxxx"), round_type = "sas")
  })
})


test_that("jjcsformats NA works", {
  ## handling NAs

  expect_snapshot({
    format_value(NA, jjcsformat_xx("xx."), round_type = "sas", na_str = "-")
    format_value(NA, jjcsformat_xx("xx"), round_type = "sas", na_str = "-")
  })

  expect_error(
    format_value(c(1, NA), format = jjcsformat_xx("xx.x (xx.x - xx.x"), round_type = "sas"),
    "jjcsformat_xx must contain same number of xx as the number of stats"
  )

  expect_snapshot({
    format_value(
      c(1.2, NA, NA),
      jjcsformat_xx("xx.x (xx.x - xx.x)"),
      round_type = "sas",
      na_str = "NE"
    )
    format_value(
      c(1.2, NA, NA),
      jjcsformat_xx("xx.x (xx.x - xx.x)"),
      round_type = "sas",
      na_str = "x"
    )
    format_value(
      c(NA, NA, NA),
      jjcsformat_xx("xx.x (xx.x - xx.x)"),
      round_type = "sas",
      na_str = "x"
    )
  })

  expect_snapshot({
    format_value(
      c(NA, NA),
      format = jjcsformat_xx("xx.x - xx.x"),
      round_type = "sas",
      na_str = c("hi", "lo")
    )
    format_value(
      c(NA, 5.2),
      format = jjcsformat_xx("xx.x - xx.x"),
      round_type = "sas",
      na_str = "what"
    )
    format_value(
      c(NA, 5.2),
      format = jjcsformat_xx("xx.x - xx.x"),
      round_type = "sas",
      na_str = c("hi", "lo")
    )
    format_value(
      c(NA, NA),
      format = jjcsformat_xx("xx.x - xx.x"),
      round_type = "sas",
      na_str = "what"
    )
  })

  expect_snapshot({
    format_value(NA, format = jjcsformat_xx("xx.x"), round_type = "sas", na_str = character())
    format_value(NA, format = jjcsformat_xx("xx.x"), round_type = "sas", na_str = NA_character_)
  })

  # 3 d formats
  expect_snapshot({
    format_value(
      c(6.23, NA, NA),
      format = jjcsformat_xx("xx.x (xx.xx, xx.xx)"),
      round_type = "sas",
      na_str = "-"
    )
    format_value(
      c(NA, NA, NA),
      format = jjcsformat_xx("xx.x (xx.xx, xx.xx)"),
      round_type = "sas",
      na_str = "-"
    )
    format_value(
      c(6.23, NA, NA),
      format = jjcsformat_xx("xx.x (xx.xx, xx.xx)"),
      round_type = "sas",
      na_str = c("-", "x", "x")
    )
    format_value(
      c(6.23, NA, NA),
      format = jjcsformat_xx("xx.x (xx.xx, xx.xx)"),
      round_type = "sas",
      na_str = c("-", "x", "y")
    )
  })
})

test_that("jjcsformats count_fraction works", {
  expect_snapshot({
    format_value(cdf, format = jjcsformat_count_denom_fraction, round_type = "sas")
    format_value(cf, format = jjcsformat_count_fraction, round_type = "sas")
    format_value(cf, format = "xx (xx.x%)", round_type = "iec")
    format_value(
      c(2000, 2001, 2000 / 2001),
      format = jjcsformat_count_denom_fraction,
      round_type = "sas"
    )
    format_value(c(2000, 2000 / 2001), format = "xx (xx.x%)", round_type = "iec")
    format_value(c(1, 2001, 1 / 2001), format = jjcsformat_count_denom_fraction, round_type = "sas")
    format_value(c(1, 1 / 2001), format = "xx (xx.x%)", round_type = "iec")
    format_value(c(3, 3, 3 / 3), format = jjcsformat_count_denom_fraction, round_type = "sas")
    format_value(c(3, 3 / 3), format = "xx (xx.x%)", round_type = "iec")
    format_value(
      rep(NA, 3),
      format = jjcsformat_xx("xx.x (xx.x, xx.x)"),
      round_type = "sas",
      na_str = rep("NE", 10)
    )
    format_value(
      rep(NA, 3),
      format = jjcsformat_xx("xx.x (xx.x, xx.x)"),
      round_type = "sas",
      na_str = rep("NA", 1)
    )
    format_value(rep(NA, 3), format = jjcsformat_xx("xx.x (xx.x, xx.x)"), round_type = "sas", na_str = "NA")
    format_value(c(1, rep(NA, 2)), format = jjcsformat_xx("xx.x (xx.x, xx.x)"), round_type = "sas", na_str = rep("NE", 10))
    format_value(
      c(1, rep(NA, 2)),
      format = jjcsformat_xx("xx.x (xx.x, xx.x)"),
      round_type = "sas",
      na_str = c("ne1", "ne2", "ne3")
    )
  })
})
