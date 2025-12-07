library(rtables)

testthat::test_that("remove_col_count works", {
  adsl <- ex_adsl
  adsl$colspan_trt <- factor(
    ifelse(adsl[["ARM"]] == "B: Placebo", " ", "Active Study Agent"),
    levels = c("Active Study Agent", " ")
  )

  adsl$set2 <- "Set 2 columns"

  lyt <- basic_table(
    top_level_section_div = " ",
    show_colcounts = TRUE,
    colcount_format = "N=xx",
    round_type = "sas"
  ) %>%
    split_cols_by("colspan_trt", split_fun = trim_levels_in_group("ARM")) %>%
    split_cols_by("ARM") %>%
    split_cols_by("set2", nested = FALSE) %>%
    split_cols_by("ARM", split_fun = remove_split_levels("B: Placebo"))

  tbl <- build_table(lyt, adsl)

  tbl2 <- remove_col_count(tbl, span_label_var = "set2")

  expected <- col_counts(tbl)
  expected[4:5] <- NA

  testthat::expect_identical(col_counts(tbl2), expected)
})
