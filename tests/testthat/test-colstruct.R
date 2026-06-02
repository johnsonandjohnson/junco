library(dplyr)
library(junco)
library(tibble)


adsl_jnj <- pharmaverseadamjnj::adsl

adae_jnj <- pharmaverseadamjnj::adae


fix_usubjid <- function(adsl, newlev = "Std Of Care") {
  rws <- which(adsl$TRT01P == newlev)

  usubj_char <- as.character(adsl$USUBJID)
  subjid <- as.integer(as.character(adsl$SUBJID))
  subjid[rws] <- subjid[rws] + 1000
  substr(usubj_char, 8, 11) <- as.character(subjid)
  adsl$USUBJID <- factor(usubj_char)
  adsl$SUBJID <- factor(as.character(subjid))
  adsl
}


make_fake_adsl <- function(adsl, newlev = "Std Of Care", oldlev = "Placebo") {
  fakeyfake <- filter(adsl, TRT01P == !!oldlev)
  fakeyfake$TRT01P <- newlev
  fakeyfake$AGE <- floor(runif(NROW(fakeyfake), 30, 90))
  adsl$TRT01P <- as.character(adsl$TRT01P)
  adsl <- rbind(adsl, fakeyfake)
  adsl$TRT01P <- factor(adsl$TRT01P)
  fix_usubjid(adsl, newlev = newlev)
}


borrow_aes <- function(adae, adsl, mult = 1, newlev = "Std Of Care", oldlev = "Placebo") { # runif(1, .9, 1.1)) {
  plac_count <- sum(adae$TRT01P == oldlev, na.rm = TRUE)
  new_count <- floor(plac_count * mult)
  soc_usubjids <- as.character(adsl$USUBJID)[!is.na(adsl$TRT01P) & adsl$TRT01P == newlev]

  duprows <- sample(seq_len(NROW(adae)), new_count, replace = TRUE)

  newrws <- adae[duprows, ]
  newrws$USUBJID <- sample(soc_usubjids, NROW(newrws), replace = TRUE)
  rbind(adae, newrws)
}


trtvar <- "TRT01P"

adsl <- adsl_jnj |>
  make_fake_adsl() |>
  create_colspan_var(
    non_active_grp = c("Placebo", "Std Of Care"),
    non_active_grp_span_lbl = " ",
    active_grp_span_lbl = "Active Study Agent",
    colspan_var = "colspan_trt",
    trt_var = trtvar
  ) |>
  mutate(
    diffs_label = "Risk Difference (%) (95% CI)",
    rrisk_label = paste(!!rlang::sym(trtvar), "vs Placebo"),
    rrisk_label2 = paste(!!rlang::sym(trtvar), "vs Std of Care")
  ) |>
  select(
    USUBJID,
    !!rlang::sym(trtvar),
    colspan_trt,
    diffs_label,
    rrisk_label,
    rrisk_label2
  )


adae <- adae_jnj |>
  filter(TRTEMFL == "Y") |>
  borrow_aes(adsl) |>
  select(
    USUBJID,
    AESER,
    AESDTH,
    AESLIFE,
    AESHOSP,
    AESDISAB,
    AESCONG,
    AESMIE,
    AEACN_DECODE,
    AESEV
  ) |>
  group_by(USUBJID) |>
  mutate(maxsev = max(as.character(AESEV), na.rm = TRUE)) |>
  ungroup() |>
  mutate(maxsev = ifelse(is.na(maxsev), "Missing", maxsev)) |>
  mutate(
    maxsev = factor(maxsev, levels = c("Mild", "Moderate", "Severe", "Missing"))
  )


adae <- inner_join(adae, adsl, by = c("USUBJID"), multiple = "all")

ctrl_grp <- c("Placebo", "Std Of Care")
act_grp <- c("Xanomeline High Dose", "Xanomeline Low Dose")

colspan_trt_map <- create_colspan_map(
  adsl,
  non_active_grp = ctrl_grp,
  non_active_grp_span_lbl = " ",
  active_grp_span_lbl = "Active Study Agent",
  colspan_var = "colspan_trt",
  trt_var = trtvar
)


lvls <- levels(adsl[[trtvar]])
combodf <- tribble(
  ~valname, ~label, ~levelcombo, ~exargs,
  "all_active", "All Active", lvls[3:4], list(),
  "all_patients", "All Patients", select_all_levels, list()
)

## this one with is_control
combodf2 <- combodf
combodf2$is_control <- c(FALSE, TRUE)


comp_map <- make_dflt_comp_map(adsl, trtvar, ctrl_grp, combodf)

afun_refpath <- function(x, ref_path = "none", ...) paste(ref_path, collapse = ".")


make_expected_paths <- function(trt_map, combo_df = NULL) {
  trtvar <- names(trt_map)[2]
  spanvar <- names(trt_map)[1]
  act_span_lvl <- head(trt_map[[spanvar]], 1)
  ctrl_span_lvl <- tail(trt_map[[spanvar]], 1)
  act_grps <- trt_map[[trtvar]][trt_map[[spanvar]] == act_span_lvl]
  ctrl_grps <- trt_map[[trtvar]][trt_map[[spanvar]] == ctrl_span_lvl]
  if (!is.null(combo_df)) {
    if (!("is_control" %in% names(combo_df))) {
      combo_df$is_control <- FALSE
    }
    act_grps <- unique(c(act_grps, combodf$valname[!combo_df$is_control]))
    ctrl_grps <- unique(c(ctrl_grps, combodf$valname[combo_df$is_control]))
  }
  exp_paths <- c(
    lapply(
      act_grps,
      function(xi) c(spanvar, act_span_lvl, trtvar, xi)
    ),
    lapply(
      ctrl_grps,
      function(xj) c(spanvar, ctrl_span_lvl, trtvar, xj)
    ),
    unlist(
      recursive = FALSE,
      lapply(
        ctrl_grps,
        function(y) {
          lapply(
            act_grps,
            function(w) c("Risk Differences", "Risk Differences", trtvar, paste(w, y, sep = " vs "))
          )
        }
      )
    )
  )
  exp_paths
}

test_that("basic direct make_multicomp_splfun usage works", {
  splfun <- make_multicomp_splfun(colspan_trt_map)

  lyt <- basic_table() |>
    split_cols_by(trtvar, split_fun = splfun) |>
    analyze(trtvar, afun = afun_refpath)

  tbl <- build_table(lyt, adae, adsl)
  expect_identical(
    unclass(col_paths(tbl)), ## its an "AsIs" b/c list column in df
    unlist(
      lapply(
        ctrl_grp,
        function(nm) {
          lapply(
            act_grp,
            function(nm2) c(trtvar, paste(nm2, nm, sep = " vs "))
          )
        }
      ),
      recursive = FALSE
    )
  )

  ## ref_path argument is being set up as an extra arg correctly
  expect_equal(
    unname(unlist(cell_values(tbl))),
    paste("colspan_trt. .TRT01P", rep(ctrl_grp, times = c(2, 2)), sep = ".")
  )

  ## only non-combo ones here
  splfun2 <- make_multicomp_splfun(colspan_trt_map, comp_level_map = subset(comp_map, !active_is_combo))

  lyt2 <- basic_table() |>
    split_cols_by(trtvar, split_fun = splfun2)

  tbl2 <- build_table(lyt2, adae, adsl)
  expect_identical(col_paths(tbl), col_paths(tbl2))

  ## test both subsetting and reordering of comparisons
  splfun3 <- make_multicomp_splfun(colspan_trt_map, comp_level_map = comp_map[c(6, 2, 1), ])

  lyt3 <- basic_table() |>
    split_cols_by(trtvar, split_fun = splfun3) |>
    analyze(trtvar, afun = afun_refpath)

  tbl3 <- build_table(lyt3, adae, adsl)

  expect_identical(
    col_paths(tbl)[c(4, 2, 1)],
    col_paths(tbl3)
  )

  ## ref_path argument is (still) being set up as an extra arg correctly
  expect_equal(
    unname(unlist(cell_values(tbl3))),
    paste("colspan_trt. .TRT01P", c("Std Of Care", rep("Placebo", 2)), sep = ".")
  )

  ## warning for comparisons involving non-existent arms
  ## comp_map as defined above has combo levels that we aren't declaring here
  splfun4 <- make_multicomp_splfun(colspan_trt_map, comp_level_map = comp_map)

  lyt4 <- basic_table() |>
    split_cols_by(trtvar, split_fun = splfun4)

  expect_warning(tbl4 <- build_table(lyt4, adae, adsl))
  expect_identical(col_paths(tbl), col_paths(tbl4))
})

test_that("make_multicomp_splfun works with active combo levels", {
  splfun <- make_multicomp_splfun(colspan_trt_map, combodf, comp_map)
  lyt <- basic_table(show_colcounts = TRUE) |>
    split_cols_by(trtvar, split_fun = splfun) |>
    analyze(trtvar, afun = afun_refpath)

  tbl <- build_table(lyt, adae, adsl)
  base_counts <- setNames(as.integer(table(adsl[[trtvar]])), levels(adsl[[trtvar]]))

  expect_equal(
    col_counts(tbl),
    unname(
      rep(
        c(
          base_counts[act_grp],
          sum(base_counts[act_grp]),
          ## this includes NAs, thats 'probably fine' but something to be aware of
          ## I doubt it can be changed at this point though alternate approach is
          ## possible if needed
          nrow(adsl)
        ),
        2
      )
    )
  )
})

test_that("make_multicomp_splfun works with combo comparator levels (default comps)", {
  splfun <- make_multicomp_splfun(colspan_trt_map, combodf2)
  lyt <- basic_table(show_colcounts = TRUE) |>
    split_cols_by(trtvar, split_fun = splfun) |>
    analyze(trtvar, afun = afun_refpath)

  tbl <- build_table(lyt, adae, adsl)
  base_counts <- setNames(as.integer(table(adsl[[trtvar]])), levels(adsl[[trtvar]]))
  expect_equal(
    col_counts(tbl),
    unname(rep(c(base_counts[3:4], sum(base_counts[3:4])), 3))
  )
})


test_that("make_multicomp_splfun works with active comparators (non-default comps)", {
  splfun <-
    make_multicomp_splfun(colspan_trt_map,
      comp_level_map = data.frame(
        active = "Xanomeline High Dose",
        comparator = "Xanomeline Low Dose"
      )
    )
  lyt <- basic_table(show_colcounts = TRUE) |>
    split_cols_by(trtvar, split_fun = splfun) |>
    analyze(trtvar, afun = afun_refpath)

  tbl <- build_table(lyt, adae, adsl)
  expect_equal(
    unlist(cell_values(tbl), use.names = FALSE),
    "colspan_trt.Active Study Agent.TRT01P.Xanomeline Low Dose"
  )

  splfun2 <-
    make_multicomp_splfun(colspan_trt_map,
      comp_level_map = data.frame(
        active = c("Xanomeline High Dose", "Xanomeline Low Dose"),
        comparator = "all_patients"
      ),
      combo_levels_map = combodf
    )

  lyt2 <- basic_table(show_colcounts = TRUE) |>
    split_cols_by(trtvar, split_fun = splfun2) |>
    analyze(trtvar, afun = afun_refpath)

  tbl2 <- build_table(lyt2, adae, adsl)

  expect_identical(
    unique(unlist(cell_values(tbl2), use.names = FALSE)),
    "colspan_trt.Active Study Agent.TRT01P.all_patients"
  )
})

test_that("grouped_cols_w_diffs works", {
  lyt1 <- basic_table() |>
    grouped_cols_w_diffs(colspan_trt_map) |>
    analyze(trtvar, afun = afun_refpath)

  tbl1 <- build_table(lyt1, adae, adsl)
  spanvar <- names(colspan_trt_map)[1]
  varvar <- names(colspan_trt_map)[2]
  expect_equal(
    unclass(col_paths(tbl1)),
    c(
      lapply(
        seq_along(colspan_trt_map[[1]]),
        function(i) {
          rw <- colspan_trt_map[i, ]
          c(spanvar, rw[[spanvar]], varvar, rw[[varvar]])
        }
      ),
      unlist(
        lapply(
          ctrl_grp,
          function(z) {
            lapply(act_grp, function(w) {
              c(rep("Risk Differences", 2), trtvar, paste(w, z, sep = " vs "))
            })
          }
        ),
        recursive = FALSE
      )
    )
  )

  lyt2 <- basic_table() |>
    grouped_cols_w_diffs(colspan_trt_map, diff_cols = FALSE) |>
    analyze(trtvar, afun = afun_refpath)
  tbl2 <- build_table(lyt2, adae, adsl)

  expect_equal(
    col_paths(tbl2),
    col_paths(tbl1)[1:4]
  )

  expect_identical(
    unique(unlist(cell_values(tbl2))),
    "none"
  )

  suppressMessages(
    lyt3 <- basic_table() |>
      grouped_cols_w_diffs(colspan_trt_map, combo_map_df = combodf) |>
      analyze(trtvar, afun = afun_refpath)
  )

  tbl3 <- build_table(lyt3, adae, adsl)
  spanvar <- names(colspan_trt_map)[1]


  expect_equal(
    unclass(col_paths(tbl3)),
    make_expected_paths(colspan_trt_map, combodf)
  )

  suppressMessages(
    lyt4 <- basic_table() |>
      grouped_cols_w_diffs(colspan_trt_map, combo_map_df = combodf2) |>
      analyze(trtvar, afun = afun_refpath)
  )
  tbl4 <- build_table(lyt4, adae, adsl)

  expect_equal(
    unclass(col_paths(tbl4)),
    make_expected_paths(colspan_trt_map, combodf2)
  )


  comp_map2 <- tribble(
    ~active, ~comparator,
    "Xanomeline High Dose", "Xanomeline Low Dose",
    "Xanomeline High Dose", "Placebo",
    "Xanomeline Low Dose", "all_patients"
  )


  suppressMessages(
    lyt5 <- basic_table() |>
      grouped_cols_w_diffs(colspan_trt_map, combo_map_df = combodf2, comp_map = comp_map2)
  )


  tbl5 <- build_table(lyt5, adsl)
  expect_equal(
    col_paths(tbl5)[-7], # high vs low isn't in tbl4
    col_paths(tbl4)[c(1:6, 7, 14)]
  )

  expect_equal(
    col_paths(tbl5)[[7]],
    c("Risk Differences", "Risk Differences", "TRT01P", "Xanomeline High Dose vs Xanomeline Low Dose")
  )
})
