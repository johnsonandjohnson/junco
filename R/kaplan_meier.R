#' Survival time analysis
#'
#' @description `r lifecycle::badge('stable')`
#'
#' The analyze function [kaplan_meier()] creates a layout element to analyze
#' survival time by calculating survival time median, 2 quantiles, each with
#' their confidence intervals, and range (for all, censored, or event patients).
#' The primary analysis variable `vars` is the time variable and the secondary
#' analysis variable `is_event` indicates whether or not an event has occurred.
#'
#' @inheritParams proposal_argument_convention
#' @param control (`list`)\cr parameters for comparison details, specified by using the helper function
#'   [tern::control_surv_time()]. Some possible parameter options are:
#'   * `conf_level` (`proportion`)\cr confidence level of the interval for survival time.
#'   * `conf_type` (`string`)\cr confidence interval type. Options are 'plain' (default), 'log', or 'log-log',
#'     see more in [survival::survfit()]. Note option 'none' is not supported.
#'   * `quantiles` (`numeric`)\cr vector of length two to specify the quantiles of survival time.
#'
#' @note These functions have been forked from the `tern` package file `survival_time.R`.
#'   Here we have the additional features:
#'
#'   * Additional statistics `quantiles_lower`, `quantiles_upper`, `range_with_cens_info` are returned.
#'
#' @examples
#' library(dplyr)
#' library(tern)
#' adtte_f <- tern::tern_ex_adtte |>
#'   filter(PARAMCD == "OS") |>
#'   mutate(
#'     AVAL = tern::day2month(AVAL),
#'     is_event = CNSR == 0
#'   )
#' df <- adtte_f |> filter(ARMCD == "ARM A")
#' @keywords internal
#' @name kaplan_meier
#' @order 1
NULL

#' @describeIn kaplan_meier Statistics function which analyzes survival times using Kaplan-Meier.
#'
#' @return
#' * `s_kaplan_meier()` returns the following statistics:
#'   * `quantiles_lower`: Lower quantile estimate and confidence interval for it.
#'   * `median_ci_3d`: Median survival time and confidence interval for it.
#'   * `quantiles_upper`: Upper quantile estimate and confidence interval for it.
#'   * `range_with_cens_info`: Survival time range with censoring information.
#'
#' @importFrom survival survfit
#'
#' @keywords internal
s_kaplan_meier <- function(df, .var, is_event, control = control_surv_time()) {
  checkmate::assert_string(.var)
  assert_df_with_variables(df, list(tte = .var, is_event = is_event))
  checkmate::assert_numeric(df[[.var]], min.len = 1, any.missing = FALSE)
  checkmate::assert_logical(df[[is_event]], min.len = 1, any.missing = FALSE)

  conf_type <- control$conf_type
  conf_level <- control$conf_level

  checkmate::assert_true(control$quantiles[1] < 0.5)
  checkmate::assert_true(control$quantiles[2] > 0.5)
  quantiles <- c(control$quantiles[1], 0.5, control$quantiles[2])

  formula <- stats::as.formula(paste0("survival::Surv(", .var, ", ", is_event, ") ~ 1"))
  srv_fit <- survival::survfit(formula = formula, data = df, conf.int = conf_level, conf.type = conf_type)
  srv_qt_tab <- stats::quantile(srv_fit, probs = quantiles)
  quantiles_lower <- vapply(srv_qt_tab, "[", 1, FUN.VALUE = numeric(1))
  median_ci <- vapply(srv_qt_tab, "[", 2, FUN.VALUE = numeric(1))
  quantiles_upper <- vapply(srv_qt_tab, "[", 3, FUN.VALUE = numeric(1))

  x <- df[[.var]]
  has_event <- df[[is_event]]
  x_censored <- x[!has_event]
  x_event <- x[has_event]

  any_censored <- !all(has_event)
  no_event <- !any(has_event)

  range_censor <- range_noinf(x_censored, na.rm = TRUE)
  range_event <- range_noinf(x_event, na.rm = TRUE)
  range <- range_noinf(x, na.rm = TRUE)

  lower_censored <- any_censored &&
    (no_event || as.numeric(range_censor[1] < range_event[1]))
  upper_censored <- any_censored &&
    (no_event || as.numeric(range_censor[2] > range_event[2]))
  range_with_cens_info <- c(range, lower_censored, upper_censored)

  list(
    quantiles_lower = with_label(
      unname(quantiles_lower),
      paste0(round(quantiles[1] * 100), "th percentile (", f_conf_level(conf_level), ")")
    ),
    median_ci_3d = with_label(unname(median_ci), paste0("Median (", f_conf_level(conf_level), ")")),
    quantiles_upper = with_label(
      unname(quantiles_upper),
      paste0(round(quantiles[3] * 100), "th percentile (", f_conf_level(conf_level), ")")
    ),
    range_with_cens_info = range_with_cens_info
  )
}

#' @describeIn kaplan_meier Formatted analysis function which is used as `afun`
#'
#' @return
#' * `a_kaplan_meier()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' a_kaplan_meier(
#'   df,
#'   .var = "AVAL",
#'   is_event = "is_event"
#' )
#'
#' basic_table() |>
#'   split_cols_by(var = "ARMCD") |>
#'   add_colcounts() |>
#'   analyze(
#'     vars = "AVAL",
#'     afun = a_kaplan_meier,
#'     var_labels = "Kaplan-Meier estimate of time to event (months)",
#'     show_labels = "visible",
#'     extra_args = list(
#'       is_event = "is_event",
#'       control = control_surv_time(conf_level = 0.9, conf_type = "log-log")
#'     )
#'   ) |>
#'   build_table(df = adtte_f)
#'
#' @export
#' @order 2
a_kaplan_meier <- function(df, .var, ..., .stats = NULL, .formats = NULL, .labels = NULL, .indent_mods = NULL) {
  # Check for additional parameters to the statistics function
  dots_extra_args <- list(...)

  # Only support default stats, not custom stats
  .stats <- .split_std_from_custom_stats(.stats)$default_stats

  # Apply statistics function
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_kaplan_meier,
    custom_stat_fnc_list = NULL,
    args_list = c(df = list(df), .var = .var, dots_extra_args)
  )

  # Format according to specifications
  format_stats(
    x_stats,
    method_groups = "kaplan_meier",
    stats_in = .stats,
    formats_in = .formats,
    labels_in = .labels,
    indents_in = .indent_mods
  )
}
