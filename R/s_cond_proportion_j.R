#' Conditional proportion with adaptive CI (exact vs. Wald)
#'
#' @description r lifecycle::badge("experimental")
#'
#' Statistics function estimating a proportion along with its confidence interval,
#' automatically selecting the Clopper–Pearson "exact" method when
#' (a) the number of responders is `num_limit` (or less), (b) all subjects except
#' `num_limit` (or less) have observed have response, or (c) the observed group size is
#' less than `denom_limit`; 
#' otherwise uses the Wald method.
#'
#' This mirrors [tern::s_proportion()] usage and output but removes the `method` argument
#' and instead decides internally between the "clopper-pearson" and "wald" options.
#'
#' @inheritParams proposal_argument_convention
#'
#' @param df (`logical` or `data.frame`)\cr if only a logical vector is used,
#'   it indicates whether each subject is a responder or not. `TRUE` represents
#'   a successful outcome. If a `data.frame` is provided, the logical vector of
#'   responses must be indicated as a variable name in `.var`.
#' @param denom (`character`)\cr denominator to use for percentage and CI computation:
#'   "n" (default, number of observed records), "N_col", or "N_row". When "N_col" or
#'   "N_row" are chosen, the corresponding `.N_col` or `.N_row` are used, respectively.
#' @param long (`flag`)\cr whether a long description is required.
#' @param num_limit (`int`)\cr numerator limit to trigger the exact method.
#' @param denom_limit (`int`)\cr denominator limit to trigger the exact method.
#'
#' @return
#' Returns statistics `n_prop` (`n` responders and proportion) and `prop_ci` (proportion CI),
#' formatted consistently with [tern::s_proportion()].
#'
#' @details
#' The CI calculation itself follows the same conventions as [tern::s_proportion()]:
#' the helper functions are called with `n = denom`, so when `denom = "N_col"` or `"N_row"`
#' the interval uses those denominators.
#'
#' @examples
#' # Logical vector input
#' rsp_v <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
#' s_cond_proportion_j(rsp_v)
#'
#' # Data frame input
#' dta <- data.frame(rsp = c(TRUE, TRUE, FALSE, TRUE, FALSE, NA))
#' s_cond_proportion_j(dta, .var = "rsp")
#'
#' # Using different denominator (requires .N_col in ...)
#' s_cond_proportion_j(dta, .var = "rsp", denom = "N_col", .N_col = 10)
#'
#' @export
s_cond_proportion_j <- function(df,
                                .var,
                                conf_level = 0.95,
                                long = FALSE,
                                num_limit = 0,
                                denom_limit = 10,
                                denom = c("n", "N_col", "N_row"),
                                .N_row,
                                .N_col) {
  checkmate::assert_flag(long)
  assert_proportion_value(conf_level)
  checkmate::assert_int(num_limit, lower = 0)
  checkmate::assert_int(denom_limit, lower = 0)
  
  rsp <- if (checkmate::test_atomic_vector(df)) {
    as.logical(df)
  } else {
    as.logical(df[[.var]])
  }
  
  n_obs <- length(rsp)
  n_rsp <- sum(rsp)
  
  denom_val <- match.arg(denom) |>
    switch(
      n = n_obs,
      N_row = .N_row,
      N_col = .N_col
    )
  
  p_hat <- ifelse(denom_val > 0, n_rsp / denom_val, 0)
  
  # Adaptive method selection based on observed data and limits.
  use_exact <- (n_obs < denom_limit) || (n_rsp <= num_limit) || (n_rsp >= (n_obs - num_limit))
  method <- if (use_exact) "clopper-pearson" else "wald"
  
  prop_ci <- switch(method,
      "clopper-pearson" = prop_clopper_pearson(rsp, n = denom_val, conf_level),
      "wald" = prop_wald(rsp, n = denom_val, conf_level)
  )
  
  list(
    "n_prop" = formatters::with_label(c(n_rsp, p_hat), "Responders"),
    "prop_ci" = formatters::with_label(
      x = 100 * prop_ci,
      label = d_cond_proportion_j(
        conf_level, 
        long = long, 
        num_limit = num_limit, 
        denom_limit = denom_limit
      )
    )
  )
}

#' Description of the conditional proportion summary
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This is a helper function that describes the analysis in [s_cond_proportion_j()].
#'
#' @inheritParams s_cond_proportion_j()
#' @param long (`flag`)\cr whether a long or a short (default) description is required.
#'
#' @return String describing the analysis.
#'
#' @export
d_cond_proportion_j <- function(conf_level,
                                long = FALSE,
                                num_limit,
                                denom_limit) {
  label <- paste0(conf_level * 100, "% CI")
  
  if (long) label <- paste(label, "for Response Rates")
  
  method_part <- if (long) {
    paste0(
      "Wald if n >= ", denom_limit, 
      " and x > ", num_limit, 
      ", else Clopper-Pearson"
    )
  } else {
    "Wald / Clopper-Pearson"
  }
  
  paste0(label, " (", method_part, ")")
}

#' @describeIn estimate_proportion Formatted analysis function which is used as `afun`
#'   for conditional proportion with adaptive CI selection (exact vs. Wald).
#'
#' @return
#' * `a_cond_proportion_j()` returns the corresponding list with formatted [rtables::CellValue()].
#'
#' @examples
#' nex <- 100
#' dta <- data.frame(
#'   "rsp" = sample(c(TRUE, FALSE), nex, TRUE),
#'   "grp" = sample(c("A", "B"), nex, TRUE),
#'   "f1"  = sample(c("a1", "a2"), nex, TRUE),
#'   stringsAsFactors = TRUE
#' )
#'
#' l <- basic_table() |>
#'   split_cols_by(var = "grp") |>
#'   analyze(
#'     vars = "rsp",
#'     afun = a_cond_proportion_j,
#'     extra_args = list(
#'       conf_level = 0.90,
#'       num_limit = 0,
#'       denom_limit = 10
#'     )
#'   )
#'
#' build_table(l, df = dta)
#'
#' @export
#' @order 2
a_cond_proportion_j <- function(
    df,
    .var,
    ...,
    .stats = NULL,
    .formats = NULL,
    .labels = NULL,
    .indent_mods = NULL) {

  dots_extra_args <- list(...)
  
  # Only support default stats, not custom stats
  .stats <- .split_std_from_custom_stats(.stats)$default_stats
  
  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_cond_proportion_j,
    custom_stat_fnc_list = NULL,
    args_list = c(
      df = list(df),
      .var = .var,
      dots_extra_args
    )
  )
  
  format_stats(
    x_stats,
    method_groups = "estimate_proportion",
    stats_in = .stats,
    formats_in = .formats,
    labels_in = .labels,
    indents_in = .indent_mods
  )
}

