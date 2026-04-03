library(testthat)
library(rtables)
library(dplyr)
library(tern)

test_that("a_summarize_aval_chg_diff_j ancova in a combined column work as expected", {
  ctrl_grp <- "B: Placebo"
  adsl <- ex_adsl 
    adsl$rrisk_header <- "Difference in Mean Change (95% CI)"
    adsl$rrisk_label <- paste(adsl[["ARM"]], paste("vs", ctrl_grp))
    adsl$colspan_trt_dummy <- " "
  adsl <- adsl |> select(USUBJID, ARM, rrisk_header, rrisk_label, colspan_trt_dummy)
    
  advs <- ex_advs |>
    filter(PARAMCD %in% c("DIABP", "PULSE")) |>
    filter(AVISIT %in% c("BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15")) |>
    mutate(
      PARAMCD = droplevels(PARAMCD),
      AVISIT = droplevels(AVISIT)
    ) |> 
    left_join(adsl)

  multivars <- c("AVAL", "AVAL", "CHG")
  extra_args_3col <- list(
    d = 1,
    ancova = TRUE,
    comp_btw_group = TRUE,
    ref_path = c("ARM", "B: Placebo"),
    multivars = multivars,
    variables = list(arm = "ARM", covariates = NULL),
    method_combo = "contrasts",
    weights_combo = "equal"
  )

  add_combo <- add_combo_facet(
    "Combined",
    label = "Combined A + C",
    levels = c("A: Drug X", "C: Combination")
  )
  
  split_fun1 <- make_split_fun(post = list(add_combo)) 
  
  add_combo2 <- add_combo_facet(
    "Combined",
    label = "Combined A + C vs B: Placebo",
    levels = c("A: Drug X", "C: Combination")
  )
  rm_placebo_from_rdiff <- cond_rm_facets(
    facets = "B: Placebo",
    ancestor_pos = -1,
    value = "Difference in Mean Change (95% CI)",
    split = "rrisk_header",
    keep_matches = FALSE
  )  
  
  split_fun2 <- make_split_fun(post = list(add_combo2, rm_placebo_from_rdiff))   
  
  


  lyt <- basic_table() |>
    split_cols_by("colspan_trt_dummy") |>
    split_cols_by("ARM", split_fun = split_fun1) |>
    split_cols_by_multivar(multivars,
                           varlabels = c("n/N (%)", "Mean (95% CI)", "Mean Change From Baseline (95% CI)")
    ) |>    
    ### restart for the rrisk_header columns - note the nested = FALSE option
    ### also note the child_labels = "hidden" in both PARAM and AVISIT
    split_cols_by("rrisk_header", nested = FALSE) |>
    split_cols_by(
      "ARM",
      split_fun = split_fun2,
      labels_var = "rrisk_label",
      colcount_format = "N=xx"
    ) |>
    ### difference columns : just 1 column & analysis needs to be done on change
    split_cols_by_multivar(multivars[3], varlabels = c(" "))
  
  lyt <- lyt |> 
     split_rows_by("PARAMCD") |>
     split_rows_by("AVISIT", child_labels = "hidden") |>
   
     analyze("STUDYID", afun = a_summarize_aval_chg_diff_j, extra_args = extra_args_3col)

  tbl <- expect_silent(build_table(lyt, advs, alt_counts_df = adsl))
  expect_s4_class(tbl, "TableTree")
  expect_snapshot(tbl)



})


