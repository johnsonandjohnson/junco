#' @name a_two_tier
#'
#' @title Two Tier Analysis Function
#'
#' @author GB, WW.
#'
#' @description The analysis function used as an `afun` in \link[rtables]{analyze}.
#'   This function simulates a final additional level of nesting with a
#'   traditional analyze call inside it.
#'
#'   This makes it possible to create what *appear to be* group summary or
#'   content rows and to *optionally or conditionally* generate one or more
#'   "detail" rows underneath it.
#'
#'   For example, in a disposition table, one might want counts for completed
#'   and ongoing patients with no further detail underneath, but a breakdown of
#'   specific reasons beneath the count of patients who discontinued treatment.
#'
#' @details Both the analysis variable and `inner_var` must be factors.
#'   Detail rows are differentiated by having an indent mod of one, causing them
#'   to hang indented under their corresponding group row.
#'
#' @note In its current form, this function may not retain any formatting or
#'   labeling instructions added by `grp_fun` or `detail_fun`, and it will
#'   override any `.indent_mods` values specified by them. This behavior may
#'   change in future versions.
#'
#' @inheritParams proposal_argument_convention
#' @param inner_var (`string`)\cr single variable name to use when generating
#'   the detail rows.
#' @param drill_down_levs (`character`)\cr the level(s) of `.var` under which
#'   detail rows should be generated.
#' @param use_all_levels (`flag`)\cr controls which factor levels will be
#'   present for `inner_var` (both in `df`/`x` and in `.df_row`) when calling
#'   `detail_fun`.
#'   If `TRUE`, all levels (those present on the factor `.df_row[[inner_var]]`,
#'   *regardless if the level is observed in the row group or not) will be
#'   present when creating detail rows.
#'   Otherwise (the default), only the levels observed
#'   *anywhere in the row group, i.e., within `.df_row`* will be present.
#' @param grp_fun (`function`)\cr analysis function to be used when generating
#'   the "group summary" outer rows.
#' @param detail_fun (`function`)\cr nalysis function to be used when generating
#'   "detail" inner rows.
#' @param .alt_df_full (`dataframe`)\cr denominator dataset for fraction and
#'   relative risk calculations.\cr
#'   this argument gets populated by the rtables split machinery
#'   (see [rtables::additional_fun_params]).
#' @param ... additional arguments passed directly to `grp_fun` and `detail_fun`.
#'
#' @return A `RowsVerticalSection` object including both the group row and all
#'   detail rows, if applicable, for the facet.
#'
#' @import checkmate
#' @export
#'
#' @examples
#'
#' # Example 1
#'
#' lyt_obs_levels <- basic_table() |>
#'   split_cols_by("ARM") |>
#'   split_rows_by("EOSSTT", child_labels = "hidden") |>
#'   analyze("EOSSTT",
#'     afun = a_two_tier,
#'     extra_args = list(
#'       grp_fun = simple_analysis,
#'       detail_fun = simple_analysis,
#'       inner_var = "DCSREAS",
#'       drill_down_levs = "DISCONTINUED"
#'     )
#'   )
#'
#' tbl <- build_table(lyt_obs_levels, ex_adsl)
#' tbl
#'
#' lyt_all_levels <- basic_table() |>
#'   split_cols_by("ARM") |>
#'   split_rows_by("EOSSTT", child_labels = "hidden") |>
#'   analyze("EOSSTT",
#'     afun = a_two_tier,
#'     extra_args = list(
#'       grp_fun = simple_analysis,
#'       detail_fun = simple_analysis,
#'       inner_var = "DCSREAS",
#'       drill_down_levs = "DISCONTINUED",
#'       use_all_levels = TRUE
#'     )
#'   )
#'
#' adsl_subset <- subset(ex_adsl, DCSREAS != "ADVERSE EVENT")
#' levels(adsl_subset$DCSREAS)
#'
#' tbl_all_levels <- build_table(lyt_all_levels, adsl_subset)
#' tbl_all_levels
#'
#' tbl_obs_levels <- build_table(lyt_obs_levels, adsl_subset)
#' tbl_obs_levels
#'
#' # Example 2
#'
#' library(dplyr)
#'
#' trtvar <- "ARM"
#' ctrl_grp <- "B: Placebo"
#'
#' adsl <- ex_adsl |> select(c("USUBJID", "STRATA1", "EOSSTT", "DCSREAS", all_of(trtvar)))
#' adsl$colspan_trt <- factor(
#'   ifelse(adsl[[trtvar]] == ctrl_grp, " ", "Active Study Agent"),
#'   levels = c("Active Study Agent", " ")
#' )
#' adsl$rrisk_header <- "Risk Difference (%) (95% CI)"
#' adsl$rrisk_label <- paste(adsl[[trtvar]], paste("vs", ctrl_grp))
#'
#' colspan_trt_map <- create_colspan_map(
#'   df = adsl,
#'   non_active_grp = ctrl_grp,
#'   non_active_grp_span_lbl = " ",
#'   active_grp_span_lbl = "Active Study Agent",
#'   colspan_var = "colspan_trt",
#'   trt_var = trtvar
#' )
#'
#' a_freq_j_args <- list(
#'   .stats = "count_unique_fraction",
#'   denom = "n_altdf",
#'   ref_path = c("colspan_trt", " ", trtvar, ctrl_grp)
#' )
#'
#' two_tier_args <- list(
#'   grp_fun = a_freq_j,
#'   detail_fun = a_freq_j,
#'   inner_var = "DCSREAS",
#'   drill_down_levs = "DISCONTINUED"
#' )
#'
#' lyt_rrisk <- basic_table() |>
#'   split_cols_by("colspan_trt", split_fun = trim_levels_to_map(map = colspan_trt_map)) |>
#'   split_cols_by(trtvar) |>
#'   split_cols_by("rrisk_header", nested = FALSE) |>
#'   split_cols_by(trtvar, labels_var = "rrisk_label", split_fun = remove_split_levels(ctrl_grp)) |>
#'   split_rows_by("STRATA1") |>
#'   split_rows_by("EOSSTT", child_labels = "hidden") |>
#'   analyze("EOSSTT", afun = a_two_tier, extra_args = c(two_tier_args, a_freq_j_args))
#'
#' adsl_subset <- subset(
#'   adsl,
#'   EOSSTT != "COMPLETED" & (is.na(DCSREAS) | DCSREAS != "PROTOCOL VIOLATION")
#' )
#'
#' tbl_rrisk <- build_table(lyt_rrisk, adsl_subset, alt_counts_df = adsl_subset)
#' tbl_rrisk
#'
a_two_tier <- function(df,
                       labelstr = NULL,
                       .var,
                       .N_col,
                       .df_row,
                       inner_var,
                       drill_down_levs,
                       .spl_context,
                       use_all_levels = FALSE,
                       grp_fun,
                       detail_fun,
                       .alt_df_full = NULL,
                       ...) {
  assert_string(inner_var)
  assert_character(drill_down_levs)
  assert_flag(use_all_levels)
  assert_function(grp_fun)
  assert_function(detail_fun)

  cur_grp <- tail(.spl_context$value, 1)
  df[[.var]] <- factor(df[[.var]], levels = cur_grp)
  .df_row[[.var]] <- factor(.df_row[[.var]], levels = cur_grp)
  args <- list(
    labelstr = labelstr,
    .var = .var,
    .N_col = .N_col,
    .df_row = .df_row,
    .spl_context = .spl_context,
    .alt_df_full = .alt_df_full,
    ...
  )
  if (names(formals(grp_fun))[1] == "df") {
    args <- c(list(df = df), args)
  } else {
    args <- c(list(x = df[[.var]]), args)
  }
  cell_vals <- unclass(
    do.call(grp_fun, args)
  )
  names(cell_vals) <- attr(cell_vals, "row_label")

  ## calculate the drill-down values if necessary
  if (cur_grp %in% drill_down_levs && any(!is.na(df[[inner_var]]))) {
    ## have to make sure we use all levels for the whole row group
    ## so that each column gets the same number of values in the same order
    all_inner_levs <- levels(df[[inner_var]])
    if (use_all_levels) {
      detail_levs <- all_inner_levs
    } else {
      detail_levs <- all_inner_levs[all_inner_levs %in% all_inner_levs[.df_row[[inner_var]]]]
    }

    inner_vec <- factor(df[[inner_var]], levels = detail_levs)
    df[[inner_var]] <- factor(df[[inner_var]], levels = detail_levs)
    .df_row[[inner_var]] <- factor(.df_row[[inner_var]], levels = detail_levs)
    det_args <- list(
      .var = inner_var,
      .N_col = .N_col,
      .df_row = .df_row,
      .spl_context = .spl_context,
      .alt_df_full = .alt_df_full,
      ...
    )
    if (names(formals(detail_fun))[1] == "df") {
      det_args <- c(list(df = df), det_args)
    } else {
      det_args <- c(list(x = df[[inner_var]]), det_args)
    }

    detail_vals <- unclass(
      do.call(detail_fun, det_args)
    )

    names(detail_vals) <- attr(detail_vals, "row_label")
  } else {
    detail_vals <- list()
  }
  in_rows(
    .list = c(cell_vals, detail_vals),
    .indent_mods = c(0, rep(1, length.out = length(detail_vals)))
  )
}
