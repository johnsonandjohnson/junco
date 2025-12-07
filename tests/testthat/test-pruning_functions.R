library(dplyr)
library(tern)

# Pre-processing the table
tab <- basic_table(round_type = "sas") %>%
  split_cols_by("ARM") %>%
  split_rows_by("RACE") %>%
  split_rows_by("STRATA1") %>%
  summarize_row_groups() %>%
  analyze_vars("COUNTRY", .stats = "count_fraction") %>%
  build_table(formatters::DM)

trtvar <- "ARM"
ctrl_grp <- "B: Placebo"
ref_path <- c("colspan_trt", " ", trtvar, "B: Placebo")

#### Tests for count_pruner function####
testthat::test_that("count_pruner is identical to standard pruning in cases where we have all 0's", {
  pruning_fun <- count_pruner(cols = "ARM")
  testthat::expect_type(pruning_fun, "closure")
  result <- prune_table(tab, pruning_fun)
  expected <- prune_table(tab)
  testthat::expect_identical(result, expected)
})

testthat::test_that("count_pruner keeps everything if 0 condition is not met", {
  sub_tab <- prune_table(tab)
  result <- prune_table(sub_tab, count_pruner(cols = "ARM"))
  expected <- sub_tab
  testthat::expect_identical(result, expected)
})

testthat::test_that("count_pruner with cat_include removes text correctly", {
  sub_tab <- tab[32, ]
  result <- prune_table(sub_tab, count_pruner(cat_include = "GBR", cols = "ARM"))
  expected <- NULL
  testthat::expect_identical(result, expected)
})

testthat::test_that("count_pruner with cat_include does nothing if text is supplied and it is not text that
  is in the table", { # nolint start
  result <- prune_table(tab, count_pruner(cat_include = "XXX", cols = "ARM"))
  expected <- tab
  testthat::expect_identical(result, expected)
}) # nolint end

testthat::test_that("count_pruner does remove if text is supplied and all columns are 0", {
  sub_tab2 <- tt_at_path(
    tab,
    c("RACE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER", "STRATA1", "A")
  )
  result <- prune_table(
    sub_tab2,
    count_pruner(cat_include = c("USA", "RU", "XXX"), cols = "ARM")
  )

  rps_label <- make_row_df(sub_tab2)$label
  expected <- sub_tab2[!(rps_label %in% c("USA")), ]

  testthat::expect_identical(result, expected)
})

testthat::test_that("remove_rows does remove if text is supplied as fixed and text is matched", {
  sub_tab3 <- tt_at_path(tab, c("RACE", "ASIAN", "STRATA1", "A"))
  result <- prune_table(
    sub_tab3,
    remove_rows(removerowtext = c("USA", "RUS", "XXX"), reg_expr = FALSE)
  )

  rps_label <- make_row_df(sub_tab3)$label
  expected <- sub_tab3[!(rps_label %in% c("USA", "RUS")), ]

  testthat::expect_identical(result, expected)
})

testthat::test_that("remove_rows does remove if text is supplied as fixed and text is matched", {
  sub_tab3 <- tt_at_path(tab, c("RACE", "ASIAN", "STRATA1", "A"))
  result <- prune_table(
    sub_tab3,
    remove_rows(removerowtext = c("US", "PAK"), reg_expr = TRUE)
  )

  rps_label <- make_row_df(sub_tab3)$label
  expected <- sub_tab3[!(rps_label %in% c("USA", "RUS", "PAK")), ]

  testthat::expect_identical(result, expected)
})

testthat::test_that("remove_rows does remove if text is supplied as fixed and text is matched", {
  sub_tab3 <- tt_at_path(tab, c("RACE", "ASIAN", "STRATA1", "A"))

  result1 <- prune_table(sub_tab3, remove_rows(removerowtext = "USA"))
  result2 <- prune_table(
    sub_tab3,
    remove_rows(removerowtext = "US", reg_expr = TRUE)
  )
  result3 <- prune_table(sub_tab3, remove_rows())

  rps_label <- make_row_df(sub_tab3)$label
  expected1 <- sub_tab3[!(rps_label %in% c("USA")), ]
  expected2 <- sub_tab3[!(rps_label %in% c("USA", "RUS")), ]
  expected3 <- sub_tab3 # nothing removed

  testthat::expect_identical(result1, expected1)
  testthat::expect_identical(result2, expected2)
  testthat::expect_identical(result3, expected3)
})


testthat::test_that("keep_non_null_rows: pruning function or via keep_rows", {
  sub_tab3 <- tt_at_path(tab, c("RACE", "ASIAN", "STRATA1", "A"))

  result1 <- prune_table(sub_tab3, keep_non_null_rows)
  result2 <- prune_table(sub_tab3, prune_func = keep_rows(keep_non_null_rows))

  expected1 <- NULL
  expected2 <- sub_tab3

  testthat::expect_identical(result1, expected1)
  testthat::expect_identical(result2, expected2)
})

testthat::test_that("test keep_non_null_rows", {
  xnull_cell_fn <- function(...) {
    rcell(NULL, label = "")
  }

  tabsx <- basic_table(round_type = "sas") %>%
    split_cols_by("ARM") %>%
    split_rows_by("ARM") %>%
    analyze("ARM", afun = xnull_cell_fn, show_labels = "hidden") %>%
    analyze("STRATA1", show_labels = "hidden") %>%
    build_table(formatters::DM)

  result <- prune_table(tabsx, keep_rows(keep_non_null_rows))

  tabsx2 <- basic_table(round_type = "sas") %>%
    split_cols_by("ARM") %>%
    split_rows_by("ARM") %>%
    analyze("STRATA1") %>%
    build_table(formatters::DM)

  expected <- tabsx2

  testthat::expect_identical(result, expected)
})

testthat::test_that("bspt_pruner both fraction and diff_from_control are NULL", {
  testthat::expect_error(
    prune_table(
      tab,
      prune_func = bspt_pruner(
        fraction = NULL,
        diff_from_control = NULL,
        cols = "ARM"
      )
    ),
    "At least one of fraction or diff_from_control must be non-NULL."
  )
})

testthat::test_that("bspt_pruner both fraction and diff_from_control are NULL", {
  testthat::expect_error(
    prune_table(
      tab,
      prune_func = bspt_pruner(
        fraction = NULL,
        diff_from_control = 0.02,
        cols = "ARM"
      )
    ),
    "control must be specified when diff_from_control is not NULL."
  )
})

testthat::test_that("bspt_pruner with fraction", {
  tab_bspt_pruner <- basic_table(round_type = "sas") %>%
    split_cols_by("ARM") %>%
    analyze_vars("COUNTRY", .stats = "count_fraction") %>%
    build_table(formatters::DM)

  result <- prune_table(
    tab_bspt_pruner,
    prune_func = bspt_pruner(
      fraction = 0.10,
      diff_from_control = NULL,
      cols = "ARM"
    )
  )

  expected <- tab_bspt_pruner[c(1, 2, 3), ]

  testthat::expect_identical(result, expected)
})

testthat::test_that("bspt_pruner with fraction and diff_from_control", {
  tab_bspt_pruner <- basic_table(round_type = "sas") %>%
    split_cols_by("ARM") %>%
    analyze_vars("COUNTRY", .stats = "count_fraction") %>%
    build_table(formatters::DM)

  result <- prune_table(
    tab_bspt_pruner,
    prune_func = bspt_pruner(
      fraction = 0.10,
      diff_from_control = 0.05,
      control = "B: Placebo",
      cols = "ARM"
    )
  )

  rps_label <- make_row_df(tab_bspt_pruner)$label
  expected <- tab_bspt_pruner[rps_label %in% c("CHN"), ]

  testthat::expect_identical(result, expected)
})

testthat::test_that("bspt_pruner with fraction and diff_from_control and keeprowtext", {
  tab_bspt_pruner <- basic_table(round_type = "sas") %>%
    split_cols_by("ARM") %>%
    analyze_vars("COUNTRY", .stats = "count_fraction") %>%
    build_table(formatters::DM)

  result1 <- prune_table(
    tab_bspt_pruner,
    prune_func = bspt_pruner(
      fraction = 0.10,
      diff_from_control = 0.05,
      control = "B: Placebo",
      cols = "ARM",
      keeprowtext = "PAK"
    )
  )

  result2 <- prune_table(
    tab_bspt_pruner,
    prune_func = bspt_pruner(
      fraction = 0.10,
      diff_from_control = 0.05,
      only_more_often = FALSE,
      control = "B: Placebo",
      cols = "ARM",
      keeprowtext = "PAK"
    )
  )

  rps_label <- make_row_df(tab_bspt_pruner)$label
  expected1 <- tab_bspt_pruner[rps_label %in% c("CHN", "PAK"), ]
  expected2 <- tab_bspt_pruner[rps_label %in% c("CHN", "BRA", "PAK"), ]

  testthat::expect_identical(result1, expected1)
  testthat::expect_identical(result2, expected2)
})

testthat::test_that("count_pruner in small groups", {
  DM_sub <- subset(DM, COUNTRY %in% c("USA", "CAN")) %>%
    mutate(COUNTRY = factor(as.character(COUNTRY))) %>%
    mutate(
      colspan_trt = factor(
        ifelse(ARM == "B: Placebo", " ", "Active Study Agent"),
        levels = c("Active Study Agent", " ")
      ),
      rrisk_header = "Risk Difference (%) (95% CI)",
      rrisk_label = paste(ARM, "vs Placebo")
    )

  extra_args <- list(
    ctrl_grp = "B: Placebo",
    id = "ID",
    ref_path = ref_path
  )

  tab_bspt_pruner <- basic_table(show_colcounts = TRUE, round_type = "sas") %>%
    split_cols_by("colspan_trt", split_fun = trim_levels_in_group("ARM")) %>%
    split_cols_by("ARM") %>%
    split_cols_by("rrisk_header", nested = FALSE) %>%
    split_cols_by(
      "ARM",
      labels_var = "rrisk_label",
      split_fun = remove_split_levels("B: Placebo")
    ) %>%
    analyze("COUNTRY", afun = a_freq_j, extra_args = extra_args) %>%
    build_table(DM_sub)

  result <- prune_table(
    tab_bspt_pruner,
    prune_func = count_pruner(count = 4, cols = "ARM")
  )

  rps <- make_row_df(tab_bspt_pruner)

  expected <- tab_bspt_pruner[rps$label == "USA", ]

  testthat::expect_identical(result, expected)
})

testthat::test_that("bspt_pruner in AE like tables", {
  my_adsl <- bind_rows(
    as_tibble(cbind(ARM = rep("Group A", 100), USUBJID = paste0("A", 1:100))),
    as_tibble(cbind(ARM = rep("Group B", 100), USUBJID = paste0("B", 1:100)))
  ) %>%
    mutate(ARM = factor(ARM))

  my_adsl$colspan <- factor(
    ifelse(my_adsl[["ARM"]] == "Group B", "Control Group", " "),
    levels = c(" ", "Control Group")
  )
  my_adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
  my_adsl$rrisk_label <- paste(my_adsl[["ARM"]], "vs Group B")

  ctrl_grp <- "Group B"

  ### example with percentage ----
  ## on body system level 6 6
  ## on decod level 4 2 and 2 4

  my_adae <-
    bind_rows(
      my_adsl %>%
        filter(USUBJID %in% c(paste0("A", 1:4), paste0("B", 1:2))) %>%
        mutate(
          AEBODSYS = "BODSYS1",
          AEDECOD = "Decod 1"
        ),
      my_adsl %>%
        filter(USUBJID %in% c(paste0("A", 5:6), paste0("B", 3:6))) %>%
        mutate(
          AEBODSYS = "BODSYS1",
          AEDECOD = "Decod 2"
        ),
      my_adsl %>%
        filter(USUBJID %in% c(paste0("A", 1:6), paste0("B", 1:4))) %>%
        mutate(
          AEBODSYS = "BODSYS2",
          AEDECOD = "Decod 3"
        ),
      my_adsl %>%
        filter(USUBJID %in% c(paste0("A", 1:6), paste0("B", 1:4))) %>%
        mutate(
          AEBODSYS = "BODSYS2",
          AEDECOD = "Decod 4"
        ),
      my_adsl %>%
        filter(USUBJID %in% c(paste0("A", 1:6), paste0("B", 1:2))) %>%
        mutate(
          AEBODSYS = "BODSYS3",
          AEDECOD = "Decod 5"
        ),
      my_adsl %>%
        filter(USUBJID %in% c(paste0("A", 1:2), paste0("B", 1:6))) %>%
        mutate(
          AEBODSYS = "BODSYS3",
          AEDECOD = "Decod 6"
        ),
      my_adsl %>%
        filter(USUBJID %in% c(paste0("A", 1:8), paste0("B", 1:8))) %>%
        mutate(
          AEBODSYS = "BODSYS3",
          AEDECOD = "Decod 7"
        )
    ) %>%
    mutate(AEBODSYS = factor(AEBODSYS)) %>%
    mutate(AEDECOD = factor(AEDECOD)) %>%
    mutate(TRTEMFL = "Y")

  extra_args_rr <- list(
    method = "wald",
    ref_path = c("colspan_trt", " ", trtvar, "Group B"),
    .stats = "count_unique_fraction"
  )

  tbl1 <- basic_table(show_colcounts = TRUE, top_level_section_div = " ", round_type = "sas") %>%
    split_cols_by("colspan", split_fun = trim_levels_in_group("ARM")) %>%
    split_cols_by("ARM") %>%
    split_cols_by("rrisk_header", nested = FALSE) %>%
    split_cols_by(
      "ARM",
      labels_var = "rrisk_label",
      split_fun = remove_split_levels("Group B")
    ) %>%
    analyze(
      "TRTEMFL",
      afun = a_freq_j,
      show_labels = "hidden",
      extra_args = append(extra_args_rr, list(label = "Subjects with >=1 AE"))
    ) %>%
    split_rows_by(
      "AEBODSYS",
      split_fun = trim_levels_in_group("AEDECOD"),
      section_div = c(" "),
      nested = FALSE
    ) %>%
    summarize_row_groups(
      "AEBODSYS",
      cfun = a_freq_j,
      extra_args = extra_args_rr
    ) %>%
    analyze(
      vars = "AEDECOD",
      afun = a_freq_j,
      indent_mod = 1L,
      show_labels = "hidden",
      extra_args = extra_args_rr
    ) %>%
    build_table(my_adae, my_adsl)

  result1 <- safe_prune_table(
    tbl1,
    prune_func = bspt_pruner(
      fraction = 0.05,
      cols = "ARM",
      keeprowtext = "Subjects with >=1 AE"
    )
  )

  result2 <- safe_prune_table(
    tbl1,
    prune_func = bspt_pruner(
      fraction = 0.05,
      diff_from_control = 0.02,
      control = "Group B",
      cols = "ARM",
      keeprowtext = "Subjects with >=1 AE"
    )
  )

  result3 <- safe_prune_table(
    tbl1,
    prune_func = bspt_pruner(
      fraction = 0.05,
      diff_from_control = 0.02,
      only_more_often = FALSE,
      control = "Group B",
      cols = "ARM",
      keeprowtext = "Subjects with >=1 AE"
    )
  )

  rps_label <- make_row_df(tbl1)$label

  expected1 <- tbl1[!(rps_label %in% c("Decod 1", "Decod 2", "BODSYS1")), ]
  expected2 <- tbl1[
    !(rps_label %in% c("Decod 1", "Decod 2", "BODSYS1", "Decod 6", "Decod 7")),
  ]
  expected3 <- tbl1[
    !(rps_label %in% c("Decod 1", "Decod 2", "BODSYS1", "Decod 7")),
  ]

  testthat::expect_identical(result1, expected1)
  testthat::expect_identical(result2, expected2)
  testthat::expect_identical(result3, expected3)
})


testthat::test_that("bspt_pruner with less obvious control specifications", {
  DM_sub <- formatters::DM %>%
    mutate(COUNTRY = factor(as.character(COUNTRY))) %>%
    mutate(SEX = factor(as.character(SEX)))

  tab_bspt_pruner <- basic_table(show_colcounts = TRUE, round_type = "sas") %>%
    split_cols_by("ARM") %>%
    split_cols_by("SEX") %>%
    analyze_vars("COUNTRY", .stats = "count_fraction") %>%
    build_table(DM_sub)

  rps_label <- make_row_df(tab_bspt_pruner)$label

  testthat::expect_error(
    prune_table(
      tab_bspt_pruner,
      prune_func = bspt_pruner(
        fraction = 0.05,
        diff_from_control = 0.02,
        only_more_often = FALSE,
        control = "M",
        cols = "ARM"
      )
    ),
    "control group spec does not result in single column"
  )

  result1 <- prune_table(
    tab_bspt_pruner,
    prune_func = bspt_pruner(
      fraction = 0.10,
      diff_from_control = 0.05,
      only_more_often = TRUE,
      control = c("A: Drug X", "SEX", "F"),
      cols = c("ARM", "*", "SEX")
    )
  )

  expected1 <- tab_bspt_pruner[rps_label %in% c("BRA", "CHN", "JPN", "PAK"), ]

  result2 <- prune_table(
    tab_bspt_pruner,
    prune_func = bspt_pruner(
      fraction = 0.10,
      diff_from_control = 0.02,
      only_more_often = TRUE,
      control = c("A: Drug X", "SEX", "F"),
      cols = c("C: Combination", "SEX", "M")
    )
  )

  expected2 <- tab_bspt_pruner[rps_label %in% c("CHN", "PAK"), ]

  testthat::expect_identical(result1, expected1)
  testthat::expect_identical(result2, expected2)
})

#### Tests for safe_prune_table function####
my_DM <- formatters::DM %>%
  filter(RACE == "THIS LEAVES EMPTY DF")

my_tab <- basic_table(round_type = "sas") %>%
  split_cols_by("ARM") %>%
  analyze("AGE") %>%
  build_table(my_DM)

testthat::test_that("check that if all data is pruned leaving no rows, the outcome is the message", {
  # create an empty table tree so we can see that safe_prune_table returns the message the user specified
  table_tree <- safe_prune_table(my_tab, prune_func = prune_empty_level)
  result <- make_row_df(table_tree)$label

  expected <- " - No Data To Display - "

  testthat::expect_equal(result, expected)
})

testthat::test_that("check that if message is changed this is reflected in the final table tree", {
  # create an empty table tree so we can see that safe_prune_table returns the message the user specified
  table_tree <- safe_prune_table(
    my_tab,
    empty_msg = " - No Data To Display Here - ",
    prune_func = prune_empty_level
  )
  result <- make_row_df(table_tree)$label

  expected <- " - No Data To Display Here - "

  testthat::expect_equal(result, expected)
})

testthat::test_that("check that when spancols=TRUE is used then we have expected behaviour", {
  # create an empty table tree so we can see that safe_prune_table returns the message the user specified
  result <- safe_prune_table(
    my_tab,
    prune_func = prune_empty_level,
    spancols = TRUE
  )

  rps_label <- make_row_df(my_tab)$label

  expected <- my_tab[!(rps_label %in% c("Mean")), ]
  expected <- sanitize_table_struct(
    expected,
    empty_msg = " - No Data To Display - "
  )

  testthat::expect_equal(result, expected)
})
