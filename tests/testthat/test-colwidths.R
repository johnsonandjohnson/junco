fontspec <- font_spec("Times", 9L, 1)
ADSL <- data.frame(
  USUBJID = c(
    "XXXXX01", "XXXXX02", "XXXXX03", "XXXXX04", "XXXXX05",
    "XXXXX06", "XXXXX07", "XXXXX08", "XXXXX09", "XXXXX10"
  ),
  TRT01P = factor(
    c(
      "ARMA", "ARMB", "ARMA", "ARMB", "ARMB",
      "Placebo", "Placebo", "Placebo", "ARMA", "ARMB"
    )
  ),
  FASFL = c("Y", "Y", "Y", "Y", "N", "Y", "Y", "Y", "Y", "Y"),
  SAFFL = c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N"),
  PKFL = c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N")
)

lyt <- basic_table() |>
  split_cols_by("TRT01P") |>
  add_overall_col("Total") |>
  analyze("FASFL",
    var_labels = "Analysis set:",
    afun = a_freq_j,
    extra_args = list(label = "Full", val = "Y"),
    show_labels = "visible"
  ) |>
  analyze("SAFFL",
    var_labels = "Analysis set:",
    afun = a_freq_j,
    extra_args = list(label = "Safety", val = "Y"),
    show_labels = "visible"
  ) |>
  analyze("PKFL",
    var_labels = "Analysis set:",
    afun = a_freq_j,
    extra_args = list(label = "PK", val = "Y"),
    show_labels = "visible"
  )

tt <- build_table(lyt, ADSL)

test_that("ttype_wrap_vec works as expected", {
  result <- ttype_wrap_vec(vec = c(1, 2, 3, 4) %>% as.character(), fontspec = fontspec, width = 2)
  # TODO: how do I guess expected_result
  expected_result <- list(
    c("1"),
    c("2"),
    c("3"),
    c("4")
  )
  testthat::expect_equal(result, expected_result)
})

test_that("check_wrap_nobreak works as expected", {
  # TODO: how do I guess argument colwidths = rep(20, 5)
  result <- check_wrap_nobreak(tt = tt, colwidths = rep(20, 5), fontspec = fontspec)
  testthat::expect_true(result)

  # TODO: how do I guess argument colwidths = rep(1, 5)
  result <- check_wrap_nobreak(tt = tt, colwidths = rep(1, 5), fontspec = fontspec)
  testthat::expect_false(result)
})


test_that("def_colwidths works as expected", {
  anl <- ex_adsl
  anl <- anl[1:10, c("USUBJID", "ARM", "BMRKR1")]
  anl <- var_relabel(anl,
    USUBJID = "Unique\nSubject\nIdentifier",
    ARM = "Description\nOf\nPlanned Arm"
  )
  suppressMessages(tt2 <- as_listing(anl, key_cols = c("USUBJID")) %>%
                     add_listing_col("ARM"))

  result <- def_colwidths(tt = tt2, fontspec = fontspec)
  # TODO: how do I guess this expected result?
  expected_result <- c(67, 72, 84)
  testthat::expect_equal(result, expected_result)
})

test_that("listing_column_widths works as expected", {
  anl <- ex_adsl
  anl <- anl[1:10, c("USUBJID", "ARM", "BMRKR1")]
  anl <- var_relabel(anl,
    USUBJID = "Unique\nSubject\nIdentifier",
    ARM = "Description\nOf\nPlanned Arm"
  )
  suppressMessages(tt3 <- as_listing(anl, key_cols = c("USUBJID")) %>%
                     add_listing_col("ARM"))
  mpf <- rlistings::matrix_form(tt3)
  suppressMessages(testthat::expect_message(result <- listing_column_widths(mpf, verbose = TRUE)))
  # TODO: how do I guess this expected result?
  expected_result <- c(67, 72, 84)
  testthat::expect_equal(result, expected_result)
})

test_that("find_free_colspc works as expected", {
  anl <- ex_adsl
  anl <- anl[1:10, c("USUBJID", "ARM", "BMRKR1")]
  anl <- var_relabel(anl,
    USUBJID = "Unique\nSubject\nIdentifier",
    ARM = "Description\nOf\nPlanned Arm"
  )
  suppressMessages(tt4 <- as_listing(anl, key_cols = c("USUBJID")) %>%
                     add_listing_col("ARM"))
  mpf <- rlistings::matrix_form(tt4)

  possdf <- make_poss_wdf(mpf)

  odf <- order(possdf$col_num, possdf$colwidth)
  possdf <- possdf[odf, ]
  full_possdf <- possdf
  ## already ordered by colnum then width so this the first of each colwidth is the min width for that col
  dups <- duplicated(possdf$col_num)
  curdf <- possdf[!dups, ]
  possdf <- possdf[dups, ] ## without rows for ones in curdf

  suppressWarnings(result <- find_free_colspc(curposs = curdf, fullposs = full_possdf))

  expect_snapshot(result)
})

test_that("smart_colwidths_1page works as expected", {
  result <- smart_colwidths_1page(tt = tt, fontspec = fontspec)
  # TODO: how do I guess this expected result?
  expected_result <- c(58, 29, 29, 29, 29)
  testthat::expect_equal(result, expected_result)
  testthat::expect_equal(length(result), ncol(tt) + 1)

  result2 <- smart_colwidths_1page(tt = tt, fontspec = fontspec, col_gap = 0)
  result3 <- smart_colwidths_1page(tt = tt, fontspec = fontspec, col_gap = 2)
  testthat::expect_equal(result2 - 2, result3)
})

test_that("spaces_to_inches works as expected", {
  testthat::expect_equal(spaces_to_inches(spcs = 2, fontspec = fontspec), 2)
})

