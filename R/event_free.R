#' Workaround statistics function to time point survival estimate with CI
#'
#' This is a workaround for [tern::s_surv_timepoint()], which adds a statistic
#' containing the time point specific survival estimate together with the
#' confidence interval.
#'
#' @inheritParams proposal_argument_convention
#'
#' @return for `s_event_free`, a list as returned by the [tern::s_surv_timepoint()]
#' with an additional three-dimensional statistic `event_free_ci` which
#' combines the `event_free_rate` and `rate_ci` statistics.
#'
#' For `a_event_free`, analogous to [tern::a_surv_timepoint] but with the additional
#' three-dimensional statistic described above  available via `.stats`.
#'
#' @name event_free
#' @order 1
NULL

#' @describeIn event_free Statistics function which works like [tern::s_surv_timepoint()],
#'   the difference is that it returns the additional statistic `event_free_ci`.
#' @param time_point (`numeric`)\cr time point at which to estimate survival.
#' @param time_unit (`string`)\cr unit of time for the time point.
#' @param percent (`flag`)\cr whether to return in percent or not.
#' @export
#'
#' @examples
#' adtte_f <- tern::tern_ex_adtte |>
#'   dplyr::filter(PARAMCD == "OS") |>
#'   dplyr::mutate(
#'     AVAL = tern::day2month(AVAL),
#'     is_event = CNSR == 0
#'   )
#'
#' s_event_free(
#'   df = adtte_f,
#'   .var = "AVAL",
#'   time_point = 6,
#'   is_event = "is_event",
#'   time_unit = "month"
#' )
#' @order 3
s_event_free <- function(
    df,
    .var,
    time_point,
    time_unit,
    is_event,
    percent = FALSE,
    control = control_surv_timepoint()) {
  checkmate::assert_string(time_unit, min.chars = 1L)
  start <- s_surv_timepoint(
    df = df,
    .var = .var,
    time_point = time_point,
    is_event = is_event,
    control = control
  )
  rates <- c(start$event_free_rate, start$rate_ci)
  if (!percent) rates <- rates / 100
  unit_label <- if (percent) "(%) " else ""
  c(
    start,
    list(
      event_free_ci = with_label(
        rates,
        paste0(
          time_point,
          "-",
          time_unit,
          " event-free rate ",
          unit_label,
          "(",
          obj_label(start$rate_ci),
          ")"
        )
      )
    )
  )
}

#' @describeIn event_free Formatted analysis function which is used as `afun`.
#'
#' @export
#' @order 2
#'
#' @examples
#' adtte_f <- tern::tern_ex_adtte |>
#'   dplyr::filter(PARAMCD == "OS") |>
#'   dplyr::mutate(
#'     AVAL = tern::day2month(AVAL),
#'     is_event = CNSR == 0
#'   )
#'
#' basic_table() |>
#'   split_cols_by(var = "ARMCD") |>
#'   analyze(
#'     vars = "AVAL",
#'     afun = a_event_free,
#'     show_labels = "hidden",
#'     na_str = tern::default_na_str(),
#'     extra_args = list(
#'       time_unit = "week",
#'       time_point = 3,
#'       is_event = "is_event"
#'     )
#'   ) |>
#'   build_table(df = adtte_f)
a_event_free <- function(df, .var, ..., .stats = NULL, .formats = NULL, .labels = NULL, .indent_mods = NULL) {
  # Check for additional parameters to the statistics function
  dots_extra_args <- list(...)

  # Only support default stats, not custom stats
  .stats <- .split_std_from_custom_stats(.stats)$default_stats

  # Apply statistics function
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_event_free,
    custom_stat_fnc_list = NULL,
    args_list = c(df = list(df), .var = .var, dots_extra_args)
  )

  # Format according to specifications
  format_stats(
    x_stats,
    method_groups = "event_free",
    stats_in = .stats,
    formats_in = .formats,
    labels_in = .labels,
    indents_in = .indent_mods
  )
}
