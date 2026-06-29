library(dplyr)
library(rtables)

# Define script level parameters ----------------------

trtvar <- "ARM"
popfl <- "ITTFL"
ctrl_grp <- "B: Placebo"

# Process Data --------

adsl <- ex_adsl |>
  filter(!!rlang::sym(popfl) == "Y") |>
  select(STUDYID, USUBJID, all_of(trtvar), all_of(popfl))

adae <- ex_adae |>
  select(USUBJID, AEBODSYS, AEDECOD)

adsl$colspan_trt <- factor(
  ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
  levels = c("Active Study Agent", " ")
)


adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))


# join data together
ae <- adae |> left_join(adsl, by = c("USUBJID"))

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)

ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)


# split_funs for combos -----------------

# Set up levels and label for the required combined columns
add_combo <- add_combo_facet(
  "Combined",
  label = "Combined A + C",
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

# Set up levels and label for the required combined columns
add_combo2 <- add_combo_facet(
  "Combined2",
  label = "Combined vs Placebo",
  levels = c("A: Drug X", "C: Combination")
)

# choose if any facets need to be removed - e.g remove the combined column for placebo
rm_placebo_riskdiff <- cond_rm_facets(
  facets = ctrl_grp,
  ancestor_pos = NA,
  value = "Risk Difference (%) (95% CI)",
  split = "rrisk_header"
)

mysplitf <- make_split_fun(post = list(add_combo2, rm_placebo_riskdiff))


# Define layout  -----------

lyt <- basic_table(
  top_level_section_div = " ",
  show_colcounts = TRUE,
  colcount_format = "N=xx"
) |>
  split_cols_by(
    "colspan_trt",
    split_fun = trim_levels_to_map(map = colspan_trt_map)
  ) |>
  split_cols_by(trtvar, split_fun = mysplit) |>
  add_overall_col("All Patients")


lyt_diffcols <- lyt |>
  split_cols_by("rrisk_header", nested = FALSE) |>
  split_cols_by(
    trtvar,
    labels_var = "rrisk_label",
    split_fun = mysplitf
  )

# custom afun -----------------------
testafun <- function(df,
                     .var,
                     .df_row,
                     .spl_context,
                     ref_path,
                     riskdiff = TRUE,
                     riskdiff_setup = c("horizontal", "vertical")) {
  ref_info <- get_ref_info_expanded(
    df,
    .var,
    .df_row,
    .spl_context,
    ref_path,
    riskdiff,
    riskdiff_setup
  )

  perform_vs_ref_stats <- ref_info$perform_vs_ref_stats
  ref_col_expr <- ref_info$ref_col_expr
  .in_ref_col <- ref_info$.in_ref_col

  in_rows(
    .list = list(
      .in_ref_col,
      perform_vs_ref_stats
    ),
    .labels = c("in_ref_col", "vs_ref_stats")
  )
}


# test different scenarios
test_that("layout with horizontal risk diff columns, ref_path spec 'colspan_trt' '' 'ARM' 'B: Placebo'", {
  tbl <- lyt_diffcols |>
    analyze(
      vars = "STUDYID", afun = testafun,
      extra_args = list(
        ref_path = ref_path,
        riskdiff = TRUE,
        riskdiff_setup = "horizontal"
      )
    ) |>
    build_table(ae, adsl)

  tbl
  expect_snapshot(cran = TRUE, tbl)
})

test_that("layout with horizontal risk diff columns, incomplete ref_path 'ARM' 'B: Placebo'", {
  tbl <- lyt_diffcols |>
    analyze(
      vars = "STUDYID", afun = testafun,
      extra_args = list(
        ref_path = ref_path[3:4],
        riskdiff = TRUE,
        riskdiff_setup = "horizontal"
      )
    ) |>
    build_table(ae, adsl)

  tbl
  expect_snapshot(cran = TRUE, tbl)
})

test_that("layout with horizontal risk diff columns, ref_path spec 'colspan_trt' '' 'ARM' 'B: Placebo' but vertical riskdiff_setup", {
  tbl <- lyt_diffcols |>
    analyze(
      vars = "STUDYID", afun = testafun,
      extra_args = list(
        ref_path = ref_path,
        riskdiff = TRUE,
        riskdiff_setup = "vertical"
      )
    ) |>
    build_table(ae, adsl)

  tbl
  expect_snapshot(cran = TRUE, tbl)
})

test_that("layout with core columns, and vertical riskdiff_setup", {
  tbl <- lyt |>
    analyze(
      vars = "STUDYID", afun = testafun,
      extra_args = list(
        ref_path = ref_path,
        riskdiff = TRUE,
        riskdiff_setup = "vertical"
      )
    ) |>
    build_table(ae, adsl)

  tbl
  expect_snapshot(cran = TRUE, tbl)
})

test_that("layout with core columns, vertical riskdiff_setup riskdiff = FALSE", {
  tbl <- lyt |>
    analyze(
      vars = "STUDYID", afun = testafun,
      extra_args = list(
        ref_path = ref_path,
        riskdiff = FALSE,
        riskdiff_setup = "vertical"
      )
    ) |>
    build_table(ae, adsl)

  tbl
  expect_snapshot(cran = TRUE, tbl)
})

test_that("layout with core columns, vertical riskdiff_setup incomplete ref_path spec 'ARM' 'B: Placebo'", {
  tbl <- lyt |>
    analyze(
      vars = "STUDYID", afun = testafun,
      extra_args = list(
        ref_path = ref_path[3:4],
        riskdiff = TRUE,
        riskdiff_setup = "vertical"
      )
    ) |>
    build_table(ae, adsl)

  tbl
  expect_snapshot(cran = TRUE, tbl)
})
