library(rtables)
library(dplyr)
library(tern)


testthat::test_that("a_freq_j works (old count_unq case)", {
  adsl <- ex_adsl
  adae <- ex_adae
  adae$TRTEMFL <- "Y"

  lyt <- basic_table(
    top_level_section_div = " ",
    show_colcounts = TRUE,
    colcount_format = "N=xx"
  ) |>
    analyze(
      "TRTEMFL",
      afun = a_freq_j,
      extra_args = list(
        label = "Subjects with AE",
        .stats = c("count_unique_fraction"),
        val = "Y"
      )
    )

  tbl <- build_table(lyt, adae, adsl)

  result <- unname(unlist(cell_values(tbl)))

  n <- length(unique(adae[["USUBJID"]]))
  expected <- c(n, n / nrow(adsl))

  testthat::expect_identical(result, expected)

  lyt2 <- basic_table(
    top_level_section_div = " ",
    show_colcounts = TRUE,
    colcount_format = "N=xx"
  ) |>
    analyze(
      "TRTEMFL",
      afun = a_freq_j,
      extra_args = list(
        label = "Subjects with AE",
        .stats = c("count_unique"),
        val = "Y",
        N_only = TRUE
      )
    )

  tbl2 <- build_table(lyt2, adae, adsl)

  result2 <- unname(unlist(cell_values(tbl2)))
  expected2 <- n

  testthat::expect_identical(result2, expected2)
})

testthat::test_that("a_freq_subcol_j works (old case of cpct_subcol)", {
  adsl <- ex_adsl
  adae <- ex_adae |> dplyr::select(USUBJID, AEBODSYS, AEDECOD, AREL)
  adae$TRTEMFL <- "Y"

  adsl$COLSPAN_REL <- "AEs"
  adae <- dplyr::inner_join(adae, adsl, by = c("USUBJID"))

  # This df provides the label for the subjects with >= 1 AE row.
  Subjs_with_AEs <- tibble::tribble(
    ~value,
    ~label,
    "Y",
    "Subjects with >= 1 AE"
  )

  combodf <- tibble::tribble(
    ~valname,
    ~label,
    ~levelcombo,
    ~exargs,
    "RELATED",
    "Related AEs",
    c("AEs"),
    list()
  )

  lyt <- basic_table(
    top_level_section_div = " ",
    show_colcounts = TRUE,
    colcount_format = "N=xx"
  ) |>
    split_cols_by(
      "COLSPAN_REL",
      split_fun = add_combo_levels(combodf, trim = TRUE)
    ) |>
    split_cols_by("ARM") |>
    analyze(
      "TRTEMFL",
      afun = a_freq_subcol_j,
      extra_args = list(
        label = "Subjects with >= 1 AE",
        val = "Y",
        .stats = "count_unique_fraction",
        subcol_split = "RELATED",
        subcol_var = "AREL",
        subcol_val = "Y"
      )
    )

  tbl <- build_table(lyt, adae, adsl)

  cols <- make_col_df(tbl)
  result <- unlist(unname(cell_values(
    tbl,
    colpath = c("COLSPAN_REL", "RELATED", "ARM", "A: Drug X")
  )))
  result <- unname(result) # ! remove names.stats = "count_unique_fraction",

  n_rel_ae <- length(unique(adae[
    adae$ARM == "A: Drug X" & !is.na(adae$AREL) & adae$AREL == "Y",
  ][["USUBJID"]]))
  n_col <- length(unique(adsl[adsl$ARM == "A: Drug X", ][["USUBJID"]]))
  expected <- c(n_rel_ae, n_rel_ae / n_col)

  testthat::expect_identical(result, expected)
})

testthat::test_that("a_freq_combos_j (old cpct_filter_combos case) works", {
  adsl <- ex_adsl |>
    mutate(
      months = (EOSDY + 30) / 30.4375,
      ACAT1 = case_when(
        months <= 3 ~ "Within 3 months",
        months > 3 & months <= 12 ~ "4 to 12 months",
        months > 12 ~ "Beyond 13 months",
        ### dummy level
        .default = "Beyond 13 months"
      ),
      ACAT1 = factor(
        ACAT1,
        levels = c("Within 3 months", "4 to 12 months", "Beyond 13 months")
      )
    ) |>
    select(USUBJID, ARM, EOSDY, ACAT1, months)

  adae <- ex_adae |> dplyr::select(USUBJID, AEBODSYS, AEDECOD, ASTDY, ARM)
  adae$TRTEMFL <- "Y"
  adae$TRTEMFL <- factor(adae$TRTEMFL)

  adae <- adae |>
    # ACAT1 derivation
    mutate(months = (ASTDY + 30) / 30.4375) |>
    mutate(
      ACAT1 = case_when(
        months <= 3 ~ "Within 3 months",
        months > 3 & months <= 12 ~ "4 to 12 months",
        months > 12 ~ "Beyond 13 months",
        .default = NA_character_
      )
    ) |>
    mutate(
      ACAT1 = factor(
        ACAT1,
        levels = c(
          "Within 3 months",
          "4 to 12 months",
          "Beyond 13 months",
          NA
        )
      )
    ) |>
    select(-months) |>
    # first occurrence derivation
    arrange(USUBJID, ASTDY) |>
    group_by(USUBJID) |>
    mutate(AOCCFL = case_when(row_number() == 1 ~ "Y"))

  # This df generates facets for column space : levels from adsl ACAT1 need to be cumulative
  # subjects with duration of Within 3 months
  # also subjects that have longer duration are to be included in this column
  # the proper value in the row space (from adae onset of AE in time window) need to be
  # selected via a filter in cpct_filter_combos function.

  combodf <- tibble::tribble(
    ~valname,
    ~label,
    ~levelcombo,
    ~exargs,
    "Total",
    "Total",
    c("Within 3 months", "4 to 12 months", "Beyond 13 months"),
    list(),
    "Thru 3 months",
    "Within 3 months",
    c("Within 3 months", "4 to 12 months", "Beyond 13 months"),
    list(),
    "Thru 12 months",
    "4 to 12 months",
    c("4 to 12 months", "Beyond 13 months"),
    list(),
    "Over 13 months",
    "Beyond 13 months",
    c("Beyond 13 months"),
    list()
  )

  # This df provides the label for the subjects with >= 1 AE row.
  Subjs_with_AEs <- tibble::tribble(
    ~value,
    ~label,
    "Y",
    "Subjects with >= 1 AE"
  )

  lyt <- basic_table(top_level_section_div = " ", show_colcounts = TRUE) |>
    split_cols_by("ARM") |>
    split_cols_by(
      "ACAT1",
      split_fun = add_combo_levels(
        combosdf = combodf,
        trim = FALSE,
        keep_levels = combodf$valname
      )
    ) |>
    analyze(
      "TRTEMFL",
      nested = FALSE,
      show_labels = "hidden",
      afun = a_freq_combos_j,
      extra_args = list(
        keep_levels = "Y",
        .stats = "count_unique_fraction",
        label_map = Subjs_with_AEs,
        combosdf = combodf,
        filter_var = "ACAT1",
        do_not_filter = "Total",
        flag_var = "AOCCFL"
      )
    )

  tbl <- build_table(lyt, adae, adsl)

  result <- unlist(unname(cell_values(
    tbl,
    colpath = c("ARM", "A: Drug X", "ACAT1", "Thru 12 months")
  )))
  result <- unname(result) # ! remove names

  n_denom <- nrow(
    unique(
      adsl |>
        filter(
          ARM == "A: Drug X" &
            ACAT1 %in% c("4 to 12 months", "Beyond 13 months")
        ) |>
        select(USUBJID)
    )
  )

  n_val <- nrow(unique(
    adae |>
      filter(
        ARM == "A: Drug X" & ACAT1 %in% c("4 to 12 months") & AOCCFL == "Y"
      ) |>
      select(USUBJID)
  ))

  expected <- c(n_val, n_val / n_denom)

  testthat::expect_identical(result, expected)
})

testthat::test_that("`a_freq_j()` works", {
  adsl <- ex_adsl

  trtvar <- "ARM"
  ctrl_grp <- "B: Placebo"

  adsl$colspan_trt <- factor(
    ifelse(adsl[["ARM"]] == "B: Placebo", " ", "Active Study Agent"),
    levels = c("Active Study Agent", " ")
  )

  adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
  adsl$rrisk_label <- paste(adsl[["ARM"]], "vs Placebo")

  advs <- ex_advs |> select(USUBJID, PARAMCD, PARAM, AVISIT, ANRIND)
  advs <- dplyr::inner_join(advs, adsl, by = c("USUBJID"))

  advs <- advs[advs$AVISIT %in% c("BASELINE", "WEEK 1 DAY 8"), ]
  advs <- advs[advs$PARAMCD %in% c("DIABP", "PULSE"), ]
  advs$AVISIT <- factor(as.character(advs$AVISIT))
  advs$PARAM <- factor(as.character(advs$PARAM))

  advs <- advs |>
    group_by(ARM, PARAMCD, AVISIT) |>
    arrange(USUBJID) |>
    mutate(id = row_number())

  # set ANRIND to missing for first 10 subjects from each arm
  advs$ANRIND[advs$id <= 10] <- NA_character_

  ref_path <- c("colspan_trt", " ", trtvar, ctrl_grp)

  extra_args_rr <- list(
    ref_path = ref_path,
    riskdiff = FALSE,
    .stats = c("count_unique_denom_fraction"),
    denom = "n_df"
  )

  lyt <- basic_table(
    show_colcounts = TRUE,
    colcount_format = "N=xx"
  ) |>
    split_cols_by("colspan_trt", split_fun = trim_levels_in_group("ARM")) |>
    split_cols_by("ARM") |>
    split_cols_by("rrisk_header", nested = FALSE) |>
    split_cols_by(
      "ARM",
      labels_var = "rrisk_label",
      split_fun = remove_split_levels("B: Placebo")
    ) |>
    split_rows_by("PARAM", label_pos = "topleft", section_div = " ") |>
    split_rows_by("AVISIT") |>
    analyze(
      "ANRIND",
      afun = a_freq_j,
      extra_args = extra_args_rr,
      show_labels = "hidden"
    )

  tbl <- build_table(lyt, advs, adsl)

  result <- cell_values(
    tbl,
    rowpath = c("PARAM", "Diastolic Blood Pressure", "AVISIT", "WEEK 1 DAY 8")
  )

  result <- sapply(result[[1]][1:3], \(col) col[[2]])

  n_expected <- as.double(
    table(
      advs |>
        ungroup() |>
        filter(
          PARAM == "Diastolic Blood Pressure" &
            AVISIT == "WEEK 1 DAY 8" &
            !is.na(ANRIND)
        ) |>
        mutate(
          ARM = factor(
            as.character(ARM),
            levels = c("A: Drug X", "C: Combination", "B: Placebo")
          )
        ) |>
        select(ARM)
    )
  )

  names(n_expected) <- NULL
  names(result) <- NULL

  testthat::expect_identical(result, n_expected)
})

testthat::test_that("a_freq_j works (old count_subject case)", {
  adsl <- ex_adsl
  adae <- ex_adae
  adae$TRTEMFL <- "Y"
  adae$TRTEMFL <- factor(adae$TRTEMFL)

  trtvar <- "ARM"

  simple_afun <- function(df, .var, id = "USUBJID", label = NULL) {
    df <- df[!is.na(.var), ]
    n <- length(unique(df[[id]]))

    rcell(n, format = "xx", label = label)
  }

  extra_args_1 <- list(
    denom_by = "SEX",
    .stats = c("n_altdf")
  )

  # scenario 1: cfun - subgroup coming from adsl, not df
  lyt1 <- basic_table(show_colcounts = TRUE) |>
    split_cols_by("ARM") |>
    split_rows_by("SEX") |>
    summarize_row_groups("SEX", cfun = a_freq_j, extra_args = extra_args_1) |>
    analyze(
      "TRTEMFL",
      afun = simple_afun,
      extra_args = list(label = "Subjects with AE")
    )

  tbl1 <- build_table(lyt1, adae, adsl)

  result1 <- unname(unlist(cell_values(
    tbl1,
    rowpath = c("SEX", "F"),
    colpath = c("ARM", "B: Placebo")
  )))

  ## 2 cell value : content + n

  n_1 <- nrow(unique(
    adsl |> filter(ARM == "B: Placebo" & SEX == "F") |> select(USUBJID)
  ))
  n_2 <- nrow(unique(
    adae |> filter(ARM == "B: Placebo" & SEX == "F") |> select(USUBJID)
  ))

  expected1 <- c(n_1, n_2)

  testthat::expect_identical(result1, expected1)

  # scenario 2: shift table for lab/vs
  advs <- ex_advs |>
    select(USUBJID, PARAMCD, PARAM, ANRIND, AVAL, BASE, ABLFL, AVISIT) |>
    filter(PARAMCD == "DIABP" & (AVISIT == "WEEK 1 DAY 8" | ABLFL == "Y"))
  ### remove subjects from advs, so that N is not same as from adsl

  advs <- advs[!(advs$USUBJID %in% adsl$USUBJID[1:10]), ]

  BNRIND <- advs |>
    filter(ABLFL == "Y") |>
    mutate(BNRIND = ANRIND) |>
    select(USUBJID, PARAMCD, BNRIND)

  advs <- left_join(advs, BNRIND, by = join_by(USUBJID, PARAMCD))

  adsl <- adsl |>
    mutate(BNRIND = "N") |>
    mutate(BNRIND = factor(BNRIND, levels = c("N", levels(advs$ANRIND))))
  adsl$BNRIND_header <- " "
  adsl$BNRIND_header2 <- "Baseline NRIND"

  advs <- advs |>
    mutate(
      BNRIND = factor(
        as.character(BNRIND),
        levels = c("N", levels(advs$ANRIND))
      )
    )

  advs <- left_join(
      advs,
      adsl %>% select(USUBJID, ARM, BNRIND_header, BNRIND_header2),
      by = "USUBJID"
    ) |>
    filter(ABLFL != "Y")

  ANRIND_levels <- levels(advs$ANRIND)

  lyt <- basic_table(show_colcounts = FALSE) |>
    ## to ensure N column is not under the Baseline column span header
    split_cols_by("BNRIND_header") |>
    split_cols_by("BNRIND", split_fun = keep_split_levels("N")) |>
    split_cols_by("BNRIND_header2", nested = FALSE) |>
    split_cols_by(
      "BNRIND",
      split_fun = make_split_fun(
        pre = list(rm_levels(excl = "N")),
        post = list(add_overall_facet("TOTAL", "Total"))
      )
    ) |>
    split_rows_by("ARM", child_labels = "hidden") |>
    # these counts will be checked as result1/expected1
    summarize_row_groups(
      var = "ARM",
      cfun = a_freq_j,
      extra_args = list(
        denom_by = "ARM",
        denom = "n_altdf",
        .stats = "n_altdf",
        restr_columns = "N",
        extrablanklineafter = "C: Combination"
      )
    ) |>
    split_rows_by("PARAM", nested = FALSE, split_fun = drop_split_levels) |>
    split_rows_by(
      "ARM",
      label_pos = "hidden",
      split_label = "Treatment Group",
      section_div = " "
    ) |>
    # these counts will be checked as result2/expected2
    summarize_row_groups(
      "ARM",
      cfun = a_freq_j,
      extra_args = list(
        denom = "n_rowdf",
        .stats = "denom",
        restr_columns = "N"
      )
    ) |>
    # these counts will be checked as result3/expected3
    analyze(
      "ANRIND",
      afun = a_freq_j,
      extra_args = list(
        denom = "n_rowdf",
        .stats = "count_unique",
        drop_levels = TRUE,
        new_levels = list(c("Total"), list(ANRIND_levels)),
        new_levels_after = TRUE,
        .indent_mods = 1L,
        restr_columns = c(toupper(ANRIND_levels), "TOTAL"),
        .alt_full_df = adsl
      )
    )

  tbl <- build_table(lyt, advs, adsl)

  rps <- make_row_df(tbl)
  cps <- make_col_df(tbl)

  result1 <- unname(unlist(cell_values(
    tbl,
    colpath = 1,
    rowpath = c("root", "ARM")
  ))[1:3])

  expected1 <- unname(unlist(as.list(table(adsl$ARM))))

  testthat::expect_identical(result1, expected1)

  result2 <- unname(unlist(cell_values(
    tbl,
    colpath = 1,
    rowpath = c(
      "root",
      "PARAM",
      "Diastolic Blood Pressure",
      "ARM",
      "C: Combination",
      "@content"
    )
  )))
  expected2 <- unname(unlist(as.list(table(
    advs |>
      filter(
        PARAMCD == "DIABP" & AVISIT == "WEEK 1 DAY 8" & ARM == "C: Combination"
      ) |>
      select(ARM)
  )))["C: Combination"])

  testthat::expect_identical(result2, expected2)

  result3 <- unlist(cell_values(
    tbl,
    colpath = cps$abs_pos[cps$label == "NORMAL"],
    rowpath = c(
      "root",
      "PARAM",
      "Diastolic Blood Pressure",
      "ARM",
      "B: Placebo",
      "ANRIND",
      "count_unique.HIGH"
    )
  ))
  expected3 <- table(
    advs |>
      filter(
        PARAMCD == "DIABP" & AVISIT == "WEEK 1 DAY 8" & ARM == "B: Placebo"
      ) |>
      select(BNRIND, ANRIND)
  )["NORMAL", "HIGH"]

  result3 <- unname(result3)
  expected3 <- unname(expected3)

  testthat::expect_identical(result3, expected3)
})


testthat::test_that("`a_freq_j()` works", {
  adsl <- ex_adsl
  adae <- ex_adae

  lyt <- basic_table(show_colcounts = TRUE) |>
    split_cols_by("ARM") |>
    analyze(
      vars = "STUDYID",
      afun = a_freq_j,
      extra_args = list(
        .stats = "n_altdf",
        label = "my label"
      )
    )

  tbl <- build_table(lyt, adae, adsl)

  expected <- col_counts(tbl)

  result <-
    cell_values(tbl) |>
    sapply(\(col) col[[1]]) |>
    as.integer()

  testthat::expect_identical(result, expected)
})


testthat::test_that("a_freq_j works (old a_countpat_newlevels case)", {
  adsl <- ex_adsl

  # introduce missing values
  adsl$BMRKR2[1:10] <- NA_character_

  new_BMRKR2_levels <- list(c("Medium - High"), list(c("MEDIUM", "HIGH")))

  lyt <- basic_table(show_colcounts = TRUE) |>
    split_cols_by("ARM") |>
    ### add extra combined level for BMRKR2
    analyze(
      vars = "BMRKR2",
      afun = a_freq_j,
      extra_args = list(
        .stats = c("count_unique_fraction"),
        .alt_df_full = adsl,
        ## END
        denom = "n_df",
        new_levels = new_BMRKR2_levels,
        .indent_mods = 1L,
        .extra_indent = TRUE,
        addstr2levs = ", n (%)"
      )
    )

  tbl <- build_table(lyt, adsl)

  rps <- make_row_df(tbl)$label

  expected_label <- paste0(c(levels(adsl$BMRKR2), "Medium - High"), ", n (%)")
  expected_label <- expected_label[c(1, 4, 2, 3)]

  result_label <- rps

  testthat::expect_identical(result_label, expected_label)

  expected_value <- table(
    adsl |>
      filter(ARM == "A: Drug X") |>
      select(BMRKR2)
  )

  denom <- sum(expected_value)

  expected_value <- c(expected_value, sum(expected_value[c("MEDIUM", "HIGH")]))

  expected_value <- expected_value[c(1, 4, 2, 3)]
  names(expected_value) <- NULL

  result_value_0 <- cell_values(tbl, colpath = c("ARM", "A: Drug X"))

  result_value_1 <- sapply(result_value_0, FUN = function(x) {
    x[[1]]
  })

  result_value_2 <- result_value_1[1, ]
  names(result_value_2) <- NULL

  testthat::expect_equal(result_value_2, expected_value)

  expected_pct <- expected_value / denom

  result_pct_2 <- result_value_1[2, ]
  names(result_pct_2) <- NULL
  testthat::expect_equal(result_pct_2, expected_pct)
})


testthat::test_that("a_summarize_ex_j works", {
  adsl <- ex_adsl
  trtvar <- "ARM"

  adsl$colspan_trt <- factor(
    ifelse(adsl[[trtvar]] == "B: Placebo", " ", "Active Study Agent"),
    levels = c("Active Study Agent", " ")
  )

  adsl$diff_header <- "Difference in Means (95% CI)"
  adsl$diff_label <- paste(adsl[[trtvar]], "vs Placebo")

  variables <- list(arm = "ARM", covariates = NULL)
  ref_path <- c("colspan_trt", "", "ARM", "B: Placebo")

  lyt <- basic_table(
    top_level_section_div = " ",
    show_colcounts = TRUE,
    colcount_format = "N=xx"
  ) |>
    split_cols_by("colspan_trt", split_fun = trim_levels_in_group("ARM")) |>
    split_cols_by("ARM") |>
    split_cols_by("diff_header", nested = FALSE) |>
    split_cols_by(
      "ARM",
      split_fun = remove_split_levels("B: Placebo"),
      labels_var = "diff_label"
    ) |>
    analyze(
      "EOSDY",
      afun = a_summarize_ex_j,
      var_labels = "Duration of treatment~[super a], ([unit])",
      show_labels = "visible",
      indent_mod = 0L,
      extra_args = list(
        daysconv = 1,
        variables = variables,
        ancova = TRUE,
        comp_btw_group = TRUE,
        ref_path = ref_path
      )
    )

  tbl <- build_table(lyt, adsl)

  rps <- make_row_df(tbl) |>
    filter(node_class == "DataRow")

  result_1 <- unname(cell_values(tbl[, c(
    "colspan_trt",
    "Active Study Agent",
    "ARM",
    "C: Combination"
  )]))

  result_1 <- unname(sapply(result_1, FUN = function(x) {
    unname(x)
  }))
  names(result_1[[4]]) <- NULL

  xx_1 <- adsl |> filter(ARM == "C: Combination" & !is.na(EOSDY))
  xx_1. <- xx_1[["EOSDY"]]

  ## quantiles rather than IQR, label misleading
  stats_1 <- tern:::s_summary(xx_1.)[c(
    "mean_sd",
    "median",
    "range",
    "quantiles",
    "sum"
  )]
  stats_1 <- unname(sapply(stats_1, FUN = function(x) {
    unname(x)
  }))
  ## to add subject years onto sum (which is the 5-th in stats)
  ## subject years (sum(x)*daysconv)/365.25)
  stats_1[[5]] <- c(stats_1[[5]], stats_1[[5]] / 365.25)

  attr(stats_1[[4]], "label") <- NULL

  result_1b <- unname(sapply(result_1, FUN = function(x) {
    unname(x)
  }))
  attr(result_1b[[4]], "label") <- NULL

  testthat::expect_identical(result_1b, stats_1)

  result_2 <- unname(cell_values(tbl[, c(
    "colspan_trt",
    " ",
    "ARM",
    "B: Placebo"
  )]))

  result_2 <- unname(sapply(result_2, FUN = function(x) {
    unname(x)
  }))
  names(result_2[[4]]) <- NULL

  xx_2 <- adsl |> filter(ARM == "B: Placebo" & !is.na(EOSDY))
  xx_2. <- xx_2[["EOSDY"]]

  ## quantiles rather than IQR, label misleading
  stats_2 <- tern:::s_summary(xx_2.)[c(
    "mean_sd",
    "median",
    "range",
    "quantiles",
    "sum"
  )]
  stats_2 <- unname(sapply(stats_2, FUN = function(x) {
    unname(x)
  }))
  ## to add subject years onto sum (which is the 5-th in stats)
  ## subject years (sum(x)*daysconv)/365.25)
  stats_2[[5]] <- c(stats_2[[5]], stats_2[[5]] / 365.25)

  attr(stats_2[[4]], "label") <- NULL

  result_2b <- unname(sapply(result_2, FUN = function(x) {
    unname(x)
  }))
  attr(result_2b[[4]], "label") <- NULL
  testthat::expect_identical(result_2b, stats_2)

  ### difference column - problem with colpath c("diff_header", "Difference in Means (95% CI)", "ARM", "C: Combination")
  # just take 5-th column, which is for C:Combination
  result_3 <- unname(cell_values(tbl[, 5]))[[1]][[1]]
  names(result_3) <- NULL
  attr(result_3, "label") <- NULL

  df <- xx_1
  .df_row <- adsl |> filter(!is.na(EOSDY))
  .ref_group <- xx_2
  .in_ref_col <- FALSE

  stats_3 <- tern:::s_ancova(
    df = df,
    .var = "EOSDY",
    .df_row = .df_row,
    variables = variables,
    .ref_group = .ref_group,
    .in_ref_col = .in_ref_col,
    conf_level = 0.95,
    interaction_y = FALSE,
    interaction_item = NULL
  )

  stats_3 <- unname(unlist(stats_3[c("lsmean_diff", "lsmean_diff_ci")]))

  testthat::expect_identical(result_3, stats_3)
})
