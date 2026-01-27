#' Shortcut Layout Function for Standard Continuous Variable Analysis
#'
#' @inheritParams proposal_argument_convention
#' @param formats (`list`)\cr formats including `mean_sd`, `median` and `range`
#'   specifications.
#'
#' @note This is used in `tefmad01` and `tefmad03a` e.g.
#' @return Modified layout.
#' @export
analyze_values <- function(lyt, vars, ..., formats) {
  checkmate::assert_list(formats)
  checkmate::assert_names(names(formats), must.include = c("mean_sd", "median", "range"))

  analyze_vars(
    lyt,
    vars = vars,
    ...,
    .stats = c("n", "mean_sd", "median", "range"),
    .formats = c(n = "xx", mean_sd = formats$mean_sd, median = formats$median, range = formats$range),
    .labels = c(n = "N", mean_sd = "Mean (SD)", median = "Median", range = "Min, Max"),
    .indent_mods = c(n = 0, mean_sd = 1, median = 1, range = 1)
  )
}
