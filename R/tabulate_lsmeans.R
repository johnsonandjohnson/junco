#' Tabulation of Least Square Means Results
#'
#' @description `r lifecycle::badge('stable')`
#'
#' These functions can be used to produce tables from LS means, e.g. from [fit_mmrm_j()]
#' or [fit_ancova()].
#'
#' @note These functions have been forked from the `tern.mmrm` package. Additional features
#'   are:
#'
#'   * Additional `ref_path` argument for [tern.mmrm::summarize_lsmeans()]
#'   * The function is more general in that it also works for LS means results from ANCOVA
#'   * Additional statistic `diff_mean_est_ci` is returned
#'   * P-value sidedness can be chosen
#'
#' @name tabulate_lsmeans
#' @examples
#' result <- fit_mmrm_j(
#'   vars = list(
#'     response = 'FEV1',
#'     covariates = c('RACE', 'SEX'),
#'     id = 'USUBJID',
#'     arm = 'ARMCD',
#'     visit = 'AVISIT'
#'   ),
#'   data = mmrm::fev_data,
#'   cor_struct = 'unstructured',
#'   weights_emmeans = 'equal'
#' )
#'
#' df <- broom::tidy(result)
#'
#' s_lsmeans(df[8, ], .in_ref_col = FALSE)
#' s_lsmeans(df[8, ], .in_ref_col = FALSE, alternative = "greater", show_relative = "increase")
#'
#' dat_adsl <- mmrm::fev_data |>
#'   dplyr::select(USUBJID, ARMCD) |>
#'   unique()
#'
#' basic_table() |>
#'   split_cols_by("ARMCD") |>
#'   add_colcounts() |>
#'   split_rows_by("AVISIT") |>
#'   analyze(
#'     "AVISIT",
#'     afun = a_lsmeans,
#'     show_labels = "hidden",
#'     na_str = tern::default_na_str(),
#'     extra_args = list(
#'       .stats = c(
#'         "n",
#'         "adj_mean_se",
#'         "adj_mean_ci",
#'         "diff_mean_se",
#'         "diff_mean_ci"
#'       ),
#'       .labels = c(
#'         adj_mean_se = "Adj. LS Mean (Std. Error)",
#'         adj_mean_ci = "95% CI",
#'         diff_mean_ci = "95% CI"
#'       ),
#'       .formats = c(adj_mean_se = jjcsformat_xx("xx.x (xx.xx)")),
#'       alternative = "greater",
#'       ref_path = c("ARMCD", result$ref_level)
#'     )
#'   ) |>
#'   build_table(
#'     df = broom::tidy(result),
#'     alt_counts_df = dat_adsl
#'   )
NULL

#' @describeIn tabulate_lsmeans Helper method (for [broom::tidy()]) to prepare a `data.frame` from an
#'   `tern_model` object containing the least-squares means and contrasts.
#' @method tidy tern_model
#' @importFrom generics tidy
#' @export
tidy.tern_model <- function(x, ...) {
  # nolint
  vars <- x$vars
  estimates <- x$lsmeans$estimates
  df <- if (is.null(vars$arm)) {
    nams <- names(estimates)
    to_rename <- match(c("estimate", "se", "df", "lower_cl", "upper_cl"), nams)
    names(estimates)[to_rename] <- paste0(names(estimates)[to_rename], "_est")
    estimates
  } else {
    contrasts <- x$lsmeans$contrasts
    merge(x = estimates, y = contrasts, by = c(vars$visit, vars$arm), suffixes = c("_est", "_contr"), all = TRUE)
  }
  df[[vars$arm]] <- factor(df[[vars$arm]], levels = levels(estimates[[vars$arm]]))
  df[[vars$visit]] <- factor(df[[vars$visit]], levels = levels(estimates[[vars$visit]]))
  df$conf_level <- x$conf_level
  if (!is.null(x$mse)) {
    df$mse <- x$mse[df[[vars$visit]]]
  }
  if (!is.null(x$df)) {
    df$df <- x$df[df[[vars$visit]]]
  }
  df
}

#' @describeIn tabulate_lsmeans Statistics function which is extracting estimates from a tidied least-squares means
#'   data frame.
#' @inheritParams proposal_argument_convention
#' @export
s_lsmeans <- function(df,
                      .in_ref_col,
                      alternative = c("two.sided", "less", "greater"),
                      show_relative = c("reduction", "increase")) {
  alternative <- match.arg(alternative)
  show_relative <- match.arg(show_relative)
  if_not_ref <- function(x) if (.in_ref_col) character() else x
  list(
    n = df$n,
    adj_mean_se = c(df$estimate_est, df$se_est),
    adj_mean_ci = with_label(
      c(df$lower_cl_est, df$upper_cl_est),
      paste0("Adjusted Mean ", f_conf_level(df$conf_level))
    ),
    adj_mean_est_ci = with_label(
      c(df$estimate_est, df$lower_cl_est, df$upper_cl_est),
      paste0("Adjusted Mean (", f_conf_level(df$conf_level), ")")
    ),
    diff_mean_se = if_not_ref(c(df$estimate_contr, df$se_contr)),
    diff_mean_ci = with_label(
      if_not_ref(c(df$lower_cl_contr, df$upper_cl_contr)),
      paste0("Difference in Adjusted Means ", f_conf_level(df$conf_level))
    ),
    diff_mean_est_ci = with_label(
      if_not_ref(c(df$estimate_contr, df$lower_cl_contr, df$upper_cl_contr)),
      paste0("Difference in Adjusted Means (", f_conf_level(df$conf_level), ")")
    ),
    change = switch(
      show_relative,
      reduction = with_label(if_not_ref(df$relative_reduc), "Relative Reduction (%)"),
      increase = with_label(if_not_ref(-df$relative_reduc), "Relative Increase (%)")
    ),
    p_value = if_not_ref(switch(
      alternative,
      two.sided = with_label(df$p_value, "2-sided p-value"),
      less = with_label(df$p_value_less, "1-sided p-value (less)"),
      greater = with_label(df$p_value_greater, "1-sided p-value (greater)")
    ))
  )
}

#' @describeIn tabulate_lsmeans Formatted Analysis function to be used as `afun`
#' @export
a_lsmeans <- function(df,
                      ref_path,
                      .spl_context,
                      ...,
                      .stats = NULL,
                      .formats = NULL,
                      .labels = NULL,
                      .indent_mods = NULL) {
  # Check for additional parameters to the statistics function
  dots_extra_args <- list(...)

  # Only support default stats, not custom stats
  .stats <- .split_std_from_custom_stats(.stats)$default_stats

  # Obtain reference column information
  ref <- get_ref_info(ref_path, .spl_context)

  # Apply statistics function
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_lsmeans,
    custom_stat_fnc_list = NULL,
    args_list = c(df = list(df), .in_ref_col = ref$in_ref_col, dots_extra_args)
  )

  # Format according to specifications
  format_stats(
    x_stats,
    method_groups = "tabulate_lsmeans",
    stats_in = .stats,
    formats_in = .formats,
    labels_in = .labels,
    indents_in = .indent_mods
  )
}
