test_that("a_summarize_aval_chg_diff_j comp_btw_group = FALSE works as expected", {
  library(rtables)
  library(dplyr)
  library(tern)
  
  adsl <- ex_adsl
  advs <- ex_advs %>% 
    filter(PARAMCD %in% c("DIABP", "PULSE")) %>% 
    filter(AVISIT %in% c("BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15")) %>% 
    mutate(PARAMCD = droplevels(PARAMCD),
           AVISIT = droplevels(AVISIT))
  
  multivars <- c("AVAL", "AVAL", "CHG")
  extra_args_3col <- list(d = 1, 
                          ancova = FALSE, 
                          comp_btw_group = TRUE,
                          ref_path = c("ARM", "B: Placebo"),
                          multivars = multivars)  
  
  
  lyt <- basic_table() %>% 
    split_cols_by("ARM") %>% 
    split_rows_by("PARAMCD") %>% 
    split_rows_by("AVISIT", child_labels = "hidden") %>% 
    split_cols_by_multivar(multivars, varlabels = c("n/N (%)", "Mean (95% CI)", "Mean Change From Baseline (95% CI)")) %>%
    analyze("STUDYID", afun = a_summarize_aval_chg_diff_j, extra_args = extra_args_3col)
  
  tbl <- expect_silent(build_table(lyt, advs, alt_counts_df = adsl))
  
  # this one also ran fine prior hotfix
  extra_args_3col2 <- list(d = 1, 
                           ancova = FALSE, 
                           comp_btw_group = FALSE,
                           multivars = multivars)  
  
  
  lyt2 <- basic_table() %>% 
    split_cols_by("ARM") %>% 
    split_rows_by("PARAMCD") %>% 
    split_rows_by("AVISIT", child_labels = "hidden") %>% 
    split_cols_by_multivar(multivars, varlabels = c("n/N (%)", "Mean (95% CI)", "Mean Change From Baseline (95% CI)")) %>%
    analyze("STUDYID", afun = a_summarize_aval_chg_diff_j, extra_args = extra_args_3col2)
  
  tbl2 <- expect_silent(build_table(lyt2, advs, alt_counts_df = adsl) )
  
})

test_that("hotfix: a_summarize_aval_chg_diff_j t-test sparse data works as expected", {
  library(rtables)
  library(dplyr)
  library(tern)
  
  ctrl_grp <- "B: Placebo"
  trtvar<- "ARM"
  
  adsl <- ex_adsl %>% 
    select(STUDYID, USUBJID, ARM) %>% 
    mutate(colspan_trt = factor(ifelse(ARM == ctrl_grp, " ", "Active treatment"),
                                levels = c(" ", "Active treatment")),
           rrisk_header = "Difference in Mean Change (95% CI)",
           rrisk_label = paste0(ARM, " vs ", ctrl_grp))
  
  advs <- ex_advs %>% 
    filter(PARAMCD %in% c("DIABP", "PULSE")) %>% 
    filter(AVISIT %in% c("BASELINE", "WEEK 1 DAY 8", "WEEK 2 DAY 15")) %>% 
    mutate(PARAMCD = droplevels(PARAMCD),
           AVISIT = droplevels(AVISIT))
  
  advs <- advs %>%
    inner_join(adsl)
  
  # introduce sparse data for DIABP at WEEK 1 DAY 8
  # keep 2 records in A: Drug X, and 2 records in B: Placebo with values c(50, 50) and c(45, 45)
  select_sub <- adsl %>%
    group_by(ARM) %>%
    slice_head(n = 2) %>%
    ungroup() %>% 
    pull(USUBJID)
  
  advs_2 <- advs %>% 
    mutate(AVAL = case_when(PARAMCD == "DIABP" & AVISIT == "WEEK 1 DAY 8" & !(USUBJID %in% select_sub) ~ NA_real_,
                            PARAMCD == "DIABP" & AVISIT == "WEEK 1 DAY 8" & (USUBJID %in% select_sub) & ARM == "A: Drug X" ~ 50,
                            PARAMCD == "DIABP" & AVISIT == "WEEK 1 DAY 8" & (USUBJID %in% select_sub) & ARM == "B: Placebo" ~ 45,
                            TRUE ~ AVAL)) %>% 
    mutate(CHG = case_when(PARAMCD == "DIABP" & AVISIT == "WEEK 1 DAY 8" & !(USUBJID %in% select_sub) ~ NA_real_,
                           PARAMCD == "DIABP" & AVISIT == "WEEK 1 DAY 8" & (USUBJID %in% select_sub) & ARM == "A: Drug X" ~ -5,
                           PARAMCD == "DIABP" & AVISIT == "WEEK 1 DAY 8" & (USUBJID %in% select_sub) & ARM == "B: Placebo" ~ -7,
                           TRUE ~ CHG))    
  
  colspan_trt_map <- create_colspan_map(adsl,
                                        non_active_grp = ctrl_grp,
                                        non_active_grp_span_lbl = " ",
                                        active_grp_span_lbl = "Active treatment",
                                        colspan_var = "colspan_trt",
                                        trt_var = trtvar
  )  
  
  
  multivars <- c("AVAL", "AVAL", "CHG")
  extra_args_3col <- list(d = 1, 
                          ancova = FALSE, 
                          comp_btw_group = TRUE,
                          ref_path = c("colspan_trt", " ","ARM", "B: Placebo"),
                          multivars = multivars)  
  
  
  lyt <- basic_table() %>% 
    split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) %>% 
    split_cols_by("ARM", show_colcounts = TRUE) %>% 
    split_rows_by("PARAMCD") %>% 
    split_rows_by("AVISIT", child_labels = "hidden") %>% 
    split_cols_by_multivar(multivars, varlabels = c("n/N (%)", "Mean (95% CI)", "Mean Change From Baseline (95% CI)")) %>%
    
    split_cols_by("rrisk_header", nested = FALSE) %>%
    split_cols_by("ARM",
                  split_fun = remove_split_levels(ctrl_grp), labels_var = "rrisk_label",
                  show_colcounts = FALSE
    ) %>%
    ### difference columns : just 1 column & analysis needs to be done on change
    split_cols_by_multivar(multivars[3], varlabels = c(" ")) %>%    
    
    
    analyze("STUDYID", afun = a_summarize_aval_chg_diff_j, extra_args = extra_args_3col)
  
  tbl <- expect_silent(build_table(lyt, advs, alt_counts_df = adsl))
  
  tbl_2 <- expect_silent(build_table(lyt, advs_2, alt_counts_df = adsl))
})

