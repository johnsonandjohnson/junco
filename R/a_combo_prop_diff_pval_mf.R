#' @title Combined Proportion, Risk Difference, and Risk Difference Test
#'   Analysis Function with Method Selection Based on the Mantel-Fleiss
#'   Criterion
#'
#' @description
#'   Performs a combined analysis of proportions, treatment-vs-control risk
#'   differences, and tests of risk differences.
#'
#'   The method used for the risk difference and risk difference test is
#'   selected based on the Mantel-Fleiss criterion. If the Mantel-Fleiss (MF)
#'   criterion is satisfied, the Cochran-Mantel-Haenszel (CMH) method with Sato
#'   variance is used for both the risk difference and the corresponding
#'   statistical test.
#'
#'   If the Mantel-Fleiss criterion is not satisfied, an exact method is used.
#'   Specifically, the exact unconditional method is used for estimation of the
#'   risk difference, and Fisher's exact test is used for the hypothesis test.
#'
#' @inheritParams proposal_argument_convention
#' @inheritParams a_proportion_diff_mf mf_threshold exact_footnote
#'
#' @param .df_row (`data.frame`) \cr
#'   Data set containing all analysis variables across all columns for the given
#'   row split.
#'   It is required for risk difference and risk difference test calculations
#'   to obtain the reference data.
#' @param .var (`character(1)`)\cr
#'   Name of a response variable in `df` and `.df_row`.
#'   The variable must be a factor.
#' @param val (`character(1)`)\cr
#'   The value used to identify the response of interest. An observation is
#'   considered a response of interest when `df[[.var]] == val` (and
#'   `.df_row[[.var]] == val` for the reference group).
#'   Since `df[[.var]]` and `.df_row[[.var]]` must be factors, `val` must be
#'   a `character` value matching one of their levels.
#' @param colpaths (`list(3)`)\cr
#'   A named list containing elements defining the column paths corresponding
#'   to the three types of analysis. Supported names are `"prop"`, `"diff"`, and
#'   `"pval"`. Each element must contain the column path used to identify the
#'   corresponding column(s).
#'   Each column path must contain at least two elements and have an even length.
#'   For example, valid column paths include `c("ARM", "Placebo")` or
#'   `c("ARM", "*")`.
#'   See [rtables::col_paths()] for details on column path specification.
#' @param trt_var (`character(1)`)\cr
#'   The name of the treatment variable. This variable is used to identify
#'   the control group in `.df_row`.
#'   It is used only for risk difference and risk difference test calculations.
#' @param ctrl_group (`character(1)`)\cr
#'   The value of `trt_var` identifying the control treatment group.
#'   It is used only for risk difference and risk difference test calculations.
#' @param variables (`list` or `NULL`)\cr
#'   Optional stratification variables used by the Cochran-Mantel-Haenszel
#'   analysis. These are specified as `list(strata = <strata_vars>)`, where
#'   `<strata_vars>` is a character vector containing column names in `df`.
#'   All specified strata variables must be factors.
#'   Strata variables are used only for risk difference and risk difference
#'   test calculations.
#' @param .stats (`list`)\cr
#'   A named list of at most three elements specifying the statistic(s) to
#'   calculate for each type of analysis. Supported names are `"prop"`, `"diff"`,
#'   and `"pval"`.
#'   The supported values for each analysis type are determined by the `.stats`
#'   argument of [a_freq_j()], [a_proportion_diff_mf()], and
#'   [a_test_proportion_diff_mf()], respectively.
#'
#'    If a statistic is not supplied for any of the three analysis types, the
#'    following defaults are used:
#'    \itemize{
#'      \item `prop = "count_unique_denom_fraction"`
#'      \item `diff = "diff_est_ci"`
#'      \item `pval = "pval"`
#'    }
#'
#' @param .formats (`list`)\cr
#'   A named list of at most three elements specifying the formats for the
#'   statistics specified in `.stats`. Supported names are `"prop"`, `"diff"`,
#'   and `"pval"`. The format specified for each analysis type is passed to
#'   the corresponding analysis function.
#'
#' @param ... Additional arguments reserved for compatibility with the
#'   rtables analysis function interface.
#'
#' @details
#'   The analysis function determines which type of result to calculate based
#'   on the current column path specified by `colpaths`.
#'   \itemize{
#'    \item `"prop"`: calculates and displays the proportion for the current
#'    analysis facet.
#'    \item `"diff"`: calculates the treatment-vs-control proportion difference.
#'    \item `"pval"`: calculates the treatment-vs-control p-value.
#'   }
#'
#'   For proportion columns, [a_freq_j()] is used. For risk difference and
#'   p-value columns, the control group is identified from `.df_row` using
#'   `trt_var` and `ctrl_group`, and the corresponding [a_proportion_diff_mf()]
#'   or [a_test_proportion_diff_mf()] function is called.
#'
#'   The current column is identified using [in_column()] against each element
#'   of `colpaths` list.
#'   If the current column does not correspond to any of the configured `prop`,
#'   `diff`, or `pval` paths, an empty [rtables::rcell()].
#'
#'   The function first determines the current row label from `.spl_context`.
#'   If the current split represents the root of the analysis, the label
#'   `"Full analysis set"` is used; otherwise, the value of the leaf split is
#'   used.
#'
#' @return
#'   Returns the corresponding `CellValue` or `RowsVerticalSection` object with
#'   formatted results.
#'
#' @seealso [a_freq_j()], [a_proportion_diff_mf()], [a_test_proportion_diff_mf()],
#'
#' @author WW
#' @export
#' @examples
#' library(dplyr)
#'
#' adrs_f <- formatters::ex_adrs |>
#'   filter(
#'     ARM %in% c("A: Drug X", "B: Placebo"),
#'     PARAMCD == "BESRSPI"
#'   ) |>
#'   droplevels() |>
#'   mutate(
#'     rsp = factor(ifelse(AVALC == "CR", "Y", "N")),
#'     ARM_FOR_RISKCOLS = ARM # Required for Risk Difference and p-value columns
#'   )
#'
#' riskcols_combodf <- tribble(
#'   ~valname, ~label, ~levelcombo, ~exargs,
#'   "diff", "Risk Difference", "A: Drug X", list(),
#'   "pval", "p-value", "B: Placebo", list()
#' )
#'
#' col_split_fun <- add_combo_levels(
#'   riskcols_combodf,
#'   keep_levels = riskcols_combodf$valname
#' )
#'
#' # Additional arguments for a_combo_prop_diff_pval_mf().
#' extra_args <- list(
#'   colpaths = list(
#'     prop = c("ARM", "*"),
#'     diff = c("ARM_FOR_RISKCOLS", "diff"),
#'     pval = c("ARM_FOR_RISKCOLS", "pval")
#'   ),
#'   id = "SUBJID",
#'   trt_var = "ARM",
#'   ctrl_group = "B: Placebo",
#'   val = "Y",
#'   variables = list(strata = "BMRKR2"),
#'   .stats = list(diff = "diff")
#' )
#'
#' lyt <- basic_table(top_level_section_div = " ") |>
#'   split_cols_by("ARM") |>
#'   split_cols_by(
#'     "ARM_FOR_RISKCOLS",
#'     split_fun = col_split_fun,
#'     nested = FALSE
#'   ) |>
#'   analyze(
#'     "rsp",
#'     afun = a_combo_prop_diff_pval_mf,
#'     extra_args = extra_args
#'   ) |>
#'   split_rows_by(
#'     var = "SEX",
#'     split_label = "Gender",
#'     child_labels = "hidden",
#'     label_pos = "visible"
#'   ) |>
#'   analyze(
#'     vars = "rsp",
#'     afun = a_combo_prop_diff_pval_mf,
#'     extra_args = extra_args,
#'     show_labels = "hidden"
#'   )
#'
#' tbl <- build_table(lyt, adrs_f)
#' tbl
#'
a_combo_prop_diff_pval_mf <- function(df,
                                      .var,
                                      .df_row,
                                      .spl_context,
                                      val,
                                      colpaths,
                                      id = "USUBJID",
                                      trt_var,
                                      ctrl_group,
                                      variables = list(strata = NULL),
                                      mf_threshold = NULL,
                                      conf_level = 0.95,
                                      alternative = c("two.sided", "less", "greater"),
                                      exact_footnote = rtables:::RefFootnote("Exact Inference", 1L, "+"),
                                      .stats = NULL,
                                      .formats = NULL,
                                      ...) {
  checkmate::assert_list(colpaths, names = "named")
  checkmate::assert_subset(names(colpaths), c("prop", "diff", "pval"))
  checkmate::assert_character(colpaths$prop, any.missing = FALSE, min.len = 2, null.ok = TRUE)
  checkmate::assert_character(colpaths$diff, any.missing = FALSE, min.len = 2, null.ok = TRUE)
  checkmate::assert_character(colpaths$pval, any.missing = FALSE, min.len = 2, null.ok = TRUE)
  checkmate::assert_data_frame(df)
  checkmate::assert_data_frame(.df_row)
  # .var
  checkmate::assert_string(.var)
  checkmate::assert_subset(.var, colnames(df))
  checkmate::assert_subset(.var, colnames(.df_row))
  checkmate::assert_factor(df[[.var]])
  checkmate::assert_factor(.df_row[[.var]])
  # id
  checkmate::assert_string(id)
  checkmate::assert_subset(id, colnames(df))
  checkmate::assert_subset(id, colnames(.df_row))
  # trt_var
  checkmate::assert_string(trt_var)
  checkmate::assert_subset(trt_var, colnames(df))
  checkmate::assert_subset(trt_var, colnames(.df_row))

  checkmate::assert_list(.stats, names = "named", null.ok = TRUE)
  checkmate::assert_subset(names(.stats), c("prop", "diff", "pval"))
  checkmate::assert_list(.formats, names = "named", null.ok = TRUE)
  checkmate::assert_subset(names(.formats), c("prop", "diff", "pval"))

  leaf_splc <- .spl_context[nrow(.spl_context), ]
  row_label <- if (leaf_splc$split == "root" && leaf_splc$value == "root") {
    "Full analysis set"
  } else {
    leaf_splc$value
  }

  in_prop <- in_column(colpaths$prop, .spl_context)
  in_diff <- in_column(colpaths$diff, .spl_context)
  in_pval <- in_column(colpaths$pval, .spl_context)
  # Assert that we are in at most one column at a time.
  checkmate::assert_true(sum(in_prop, in_diff, in_pval) <= 1L)

  if (in_prop) {
    # NA values of df[[.var]] as dropped silently!
    # https://github.com/johnsonandjohnson/junco/issues/461
    if (is.null(.stats$prop)) {
      .stats$prop <- "count_unique_denom_fraction"
    }
    a_freq_j(
      df = df,
      .var = .var,
      val = val,
      .df_row = .df_row,
      .spl_context = .spl_context,
      id = id,
      denom = "n_df",
      riskdiff = FALSE,
      variables = NULL,
      label = row_label,
      .stats = .stats$prop,
      .formats = .formats$prop
    )
  } else if (in_diff || in_pval) {
    ref_indices <- .df_row[[trt_var]] == ctrl_group
    .ref_group <- .df_row[ref_indices, , drop = FALSE]
    if (in_diff) {
      if (is.null(.stats$diff)) {
        .stats$diff <- "diff_est_ci"
      }
      a_proportion_diff_mf(
        df,
        .var = .var,
        .in_ref_col = FALSE,
        .ref_group = .ref_group,
        val = val,
        variables = variables,
        conf_level = conf_level,
        mf_method = "cmh_sato",
        mf_threshold = mf_threshold,
        .stats = .stats$diff,
        .formats = .formats$diff,
        .labels = setNames(list(row_label), .stats$diff),
        exact_footnote = exact_footnote
      )
    } else { # In p-value column.
      if (is.null(.stats$pval)) {
        .stats$pval <- "pval"
      }
      a_test_proportion_diff_mf(
        df,
        .var = .var,
        .in_ref_col = FALSE,
        .ref_group = .ref_group,
        val = val,
        variables = variables,
        alternative = alternative,
        mf_method = "cmh_sato",
        mf_threshold = mf_threshold,
        .stats = .stats$pval,
        .formats = .formats$pval,
        .labels = setNames(list(row_label), .stats$pval),
        exact_footnote = exact_footnote
      )
    }
  } else {
    rcell(NULL, format = NULL, label = row_label)
  }
}
