test_that("a_freq_j with label_map works in a table layout as expected", {
  set.seed(12)
  dta <- data.frame(
    id = 1:100,
    rsp = factor(sample(c(TRUE, FALSE), 100, TRUE)),
    grp = factor(rep(c("A", "B"), each = 50), levels = c("A", "B"))
  )
  label_map <- data.frame(
    value = c("TRUE", "FALSE"),
    label = c("Response", "No Response")
  )
  lyt <- basic_table() |>
    split_cols_by("grp") |>
    analyze(
      "rsp",
      afun = a_freq_j,
      extra_args = list(
        label_map = label_map,
        id = "id"
      )
    )
  result <- build_table(lyt, dta)
  expect_snapshot(cran = TRUE, result)
})

test_that("a_freq_j with label_map restricts the values according to row split and label_map", {
  set.seed(12)
  dta <- data.frame(
    id = 1:100,
    visit = factor(rep(c("Baseline", "Week 1"), length.out = 100)),
    rsp = factor(sample(c("a", "b", "c", "d"), 100, TRUE)),
    grp = factor(rep(c("A", "B"), each = 50), levels = c("A", "B"))
  )
  label_map <- data.frame(
    visit = rep(c("Baseline", "Week 1"), each = 2),
    value = c("a", "b", "c", "d"),
    label = c("Response A", "Response B", "Response C", "Response D")
  )
  lyt <- basic_table() |>
    split_cols_by("grp") |>
    split_rows_by("visit") |>
    analyze(
      "rsp",
      afun = a_freq_j,
      extra_args = list(
        label_map = label_map,
        id = "id"
      )
    )
  result <- build_table(lyt, dta)
  expect_snapshot(cran = TRUE, result)
})

test_that("a_freq_j_with_exclude allows to exclude row split levels from the analysis", {
  set.seed(12)
  dta <- data.frame(
    id = 1:100,
    visit = factor(rep(c("Baseline", "Week 1"), length.out = 100)),
    rsp = factor(sample(c("a", "b", "c", "d"), 100, TRUE)),
    grp = factor(rep(c("A", "B"), each = 50), levels = c("A", "B"))
  )
  label_map <- data.frame(
    visit = rep(c("Baseline", "Week 1"), each = 2),
    value = c("a", "b", "c", "d"),
    label = c("Response A", "Response B", "Response C", "Response D")
  )
  lyt <- basic_table() |>
    split_cols_by("grp") |>
    split_rows_by("visit") |>
    analyze(
      "rsp",
      afun = a_freq_j_with_exclude,
      extra_args = list(
        label_map = label_map,
        exclude_levels = list(visit = "Week 1"),
        id = "id"
      )
    )
  result <- build_table(lyt, dta) |>
    safe_prune_table(prune_func = tern::keep_rows(keep_non_null_rows))
  expect_snapshot(cran = TRUE, result)
})

test_that("a_freq_j in specific situation error for not passing alt_counts_df", {
  library(dplyr)
  trtvar <- "ARM"
  ctrl_grp <- "B: Placebo"

  adsl <- ex_adsl |> select(c("USUBJID", "STRATA1", "EOSSTT", all_of(trtvar)))
  adsl$colspan_trt <- factor(
    ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
    levels = c("Active Study Agent", " ")
  )
  adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
  adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))

  colspan_trt_map <- create_colspan_map(
    df = adsl,
    non_active_grp = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  )

  a_freq_j_args <- list(
    .stats = "count_unique_fraction",
    ref_path = c("colspan_trt", " ", trtvar, ctrl_grp)
  )

  lyt <- basic_table(show_colcounts = TRUE) |>
    split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) |>
    split_cols_by(trtvar) |>
    split_cols_by("rrisk_header", nested = FALSE) |>
    split_cols_by(trtvar, labels_var = "rrisk_label", split_fun = remove_split_levels(ctrl_grp)) |>
    split_rows_by("STRATA1") |>
    analyze("EOSSTT", afun = a_freq_j, extra_args = a_freq_j_args)

  expect_error(
    build_table(lyt, adsl),
    "In order to get correct numbers in relative risk column"
  )

  result <- build_table(lyt, adsl, alt_counts_df = adsl)
  expect_snapshot(cran = TRUE, result)
})

test_that("a_freq_j in layout with relative risk column for combined facet", {
  library(dplyr)
  trtvar <- "ARM"
  ctrl_grp <- "B: Placebo"

  adsl <- ex_adsl |> select(c("USUBJID", "STRATA1", "EOSSTT", all_of(trtvar)))
  adsl$colspan_trt <- factor(
    ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
    levels = c("Active Study Agent", " ")
  )
  adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
  adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))

  colspan_trt_map <- create_colspan_map(
    df = adsl,
    non_active_grp = ctrl_grp,
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  )

  a_freq_j_args <- list(
    .stats = "count_unique_fraction",
    ref_path = c("colspan_trt", " ", trtvar, ctrl_grp)
  )


  # Set up levels and label for the required combined columns
  add_combo <- add_combo_facet(
    "Combined",
    label = "Combined",
    levels = setdiff(unique(adsl[[trtvar]]), ctrl_grp)
  )

  # choose if any facets need to be removed - e.g remove the combined column for placebo
  rm_combo_from_placebo <- cond_rm_facets(
    facets = "Combined",
    ancestor_pos = NA,
    value = " ",
    split = "colspan_trt"
  )

  mysplit_comb <- make_split_fun(post = list(add_combo, rm_combo_from_placebo))

  # choose if any facets need to be removed - e.g remove the combined column for placebo
  rm_combo_from_placebo2 <- cond_rm_facets(
    facets = ctrl_grp,
    ancestor_pos = NA,
    value = "Risk Difference (%) (95% CI)",
    split = "rrisk_header"
  )
  add_combo2 <- add_combo_facet(
    "Combined",
    label = paste0("Combined vs ", ctrl_grp),
    levels = setdiff(unique(adsl[[trtvar]]), ctrl_grp)
  )

  mysplit_comb2 <- make_split_fun(post = list(add_combo2, rm_combo_from_placebo2))


  lyt <- basic_table(show_colcounts = TRUE) |>
    split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) |>
    split_cols_by(trtvar, split_fun = mysplit_comb) |>
    split_cols_by("rrisk_header", nested = FALSE) |>
    split_cols_by(trtvar, labels_var = "rrisk_label", split_fun = mysplit_comb2) |>
    split_rows_by("STRATA1") |>
    analyze("EOSSTT", afun = a_freq_j, extra_args = a_freq_j_args)


  result <- build_table(lyt, adsl, alt_counts_df = adsl)

  # confirm column is not NE (NE, NE) and numbers are correct
  actual_row <- result[2, ]
  actual <- cell_values(result[2, 7])

  colcounts <- col_counts(actual_row)

  combcols <- 1:2
  ctrlcol <- 4
  ncolcomb <- sum(colcounts[combcols])
  ncolctrl <- colcounts[ctrlcol]

  countscomb <- sum(sapply(cell_values(actual_row[1, combcols]), function(v) v[["count_unique"]]), na.rm = TRUE)
  countsctrl <- cell_values(actual_row[1, ctrlcol])[[1]][["count_unique"]]

  # default method for relative risk in a_freq_j is wald stat which is based upon tern function
  expected <- tern::stat_propdiff_ci(
    x = list(countscomb),
    y = list(countsctrl),
    N_x = ncolcomb,
    N_y = ncolctrl
  )

  testthat::expect_equal(actual, expected, ignore_attr = TRUE)
})
