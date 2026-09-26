#' Conditional proportion with adaptive CI (exact vs. Wald)
#'
#' The analysis function [a_cond_proportion_j()] is used to create a layout
#' element that estimates a response proportion with confidence interval,
#' automatically selecting between exact and Wald methods based on observed
#' counts and user-defined limits.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @details
#' The statistics function mirrors [tern::s_proportion()] usage and output but
#' removes the `method` argument and decides internally between
#' "clopper-pearson" and "wald".
#'
#' The exact method is used when any of these are true:
#' (a) the number of responders is `num_limit` (or less),
#' (b) all subjects except `num_limit` (or less) have observed response,
#' or (c) the observed group size is less than `denom_limit`.
#' Depending on the `method_scope` choice, this decision is either taken
#' for each individual cell separately, or for the entire row.
#'
#' For the observed group size used for the method decision, the choice depends
#' on the `method_scope` argument:
#'
#' - If `method_scope = "cell"`, the decision is
#'   made based on the `denom` choice (so either the current cell `n`,
#'   the column total `.N_col`, or the row total `.N_row`).
#' - If on the other hand `method_scope = "row"`, the decision is always made based on
#'   the row total `.N_row`.
#'
#' For the number of responders for the method decision, either the number of responses
#' in each cell or the row total number of responders is used.
#'
#' CI computation follows [tern::s_proportion()] conventions: helper functions
#' are called with `n = denom`, so when `denom = "N_col"` or `"N_row"`, those
#' denominators are used for the interval.
#'
#' @inheritParams proposal_argument_convention
#' @param df (`logical` or `data.frame`)\cr if only a logical vector is used,
#'   it indicates whether each subject is a responder or not. `TRUE` represents
#'   a successful outcome. If a `data.frame` is provided, the logical vector of
#'   responses must be indicated as a variable name in `.var`.
#' @param .var (`string`)
#' @param conf_level (`numeric`)
#' @param denom (`character`)\cr denominator to use for percentage and CI computation:
#'   "n" (default, number of observed records), "N_col", or "N_row". When "N_col" or
#'   "N_row" are chosen, the corresponding `.N_col` or `.N_row` are used, respectively.
#' @param .N_row (`int`)
#' @param .N_col (`int`)
#' @param long (`flag`)\cr whether a long description is required.
#' @param na.rm (`flag`)\cr whether `NA` responses should be removed before analysis.
#'   If `FALSE` and `NA` values are present, an error is raised.
#' @param num_limit (`int`)\cr numerator limit to trigger the exact method.
#' @param denom_limit (`int`)\cr denominator limit to trigger the exact method.
#' @param method_scope (`string`)\cr select the CI method using counts from the
#'   current cell (`"cell"`) or all columns in the current row (`"row"`). See details.
#' @param .df_row (`data.frame`)\cr data for the current row across all columns,
#'   supplied by `rtables` when `method_scope = "row"`.
#' @param method (`string` or `NULL`)\cr selected CI method to show in the
#'   label. `NULL` retains the combined method description.
#'
#' @name cond_proportion_j
NULL

#' @describeIn cond_proportion_j Statistics function estimating a proportion
#'   along with its confidence interval, with adaptive method selection.
#'
#' @return
#' * `s_cond_proportion_j()` returns statistics `n_prop` (`n` responders and proportion)
#'   and `prop_ci` (proportion CI), formatted consistently with [tern::s_proportion()].
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
s_cond_proportion_j <- function(
  df,
  .var,
  conf_level = 0.95,
  long = FALSE,
  na.rm = TRUE,
  num_limit = 0,
  denom_limit = 10,
  denom = c("n", "N_col", "N_row"),
  .N_row,
  .N_col,
  method_scope = c("cell", "row"),
  .df_row = NULL
) {
  checkmate::assert_flag(long)
  checkmate::assert_flag(na.rm)
  tern::assert_proportion_value(conf_level)
  checkmate::assert_int(num_limit, lower = 0)
  checkmate::assert_int(denom_limit, lower = 0)
  denom <- match.arg(denom)
  method_scope <- match.arg(method_scope)

  vec <- if (checkmate::test_atomic_vector(df)) {
    df
  } else {
    tern::assert_df_with_variables(df, list(rsp = .var))
    df[[.var]]
  }
  rsp <- safe_as_logical(vec)

  if (anyNA(rsp)) {
    if (na.rm) {
      rsp <- rsp[!is.na(rsp)]
    } else {
      stop("Missing values detected in response and `na.rm = FALSE`.", call. = FALSE)
    }
  }

  n_obs <- length(rsp)
  n_rsp <- sum(rsp)

  denom_val <- match.arg(denom) |>
    switch(
      n = n_obs,
      N_row = .N_row,
      N_col = .N_col
    )

  # The denominator cannot be lower than the number of observations here, because
  # .N_row and .N_col cannot be lower.
  assert_int(denom_val, lower = n_obs)
  p_hat <- ifelse(denom_val > 0, n_rsp / denom_val, 0)

  # The method is shared by all cells in a row when requested. The CI itself
  # still uses the current cell's responses and denominator. Therefore
  # we separate out here `method_denom` and `method_rsp` for the method decision.
  if (method_scope == "row") {
    tern::assert_df_with_variables(.df_row, list(rsp = .var))
    row_rsp <- safe_as_logical(.df_row[[.var]])
    if (anyNA(row_rsp)) {
      if (na.rm) {
        row_rsp <- row_rsp[!is.na(row_rsp)]
      } else {
        stop("Missing values detected in response and `na.rm = FALSE`.", call. = FALSE)
      }
    }
    method_denom <- length(row_rsp)
    assert_true(identical(method_denom, .N_row))
    method_rsp <- sum(row_rsp)
  } else {
    method_denom <- denom_val
    method_rsp <- n_rsp
  }

  use_exact <- (method_denom < denom_limit) ||
    (method_rsp <= num_limit) ||
    (method_rsp >= (method_denom - num_limit))
  method <- if (use_exact) "clopper-pearson" else "wald"

  prop_ci <- switch(
    method,
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
        denom_limit = denom_limit,
        method = if (method_scope == "row") method else NULL
      )
    )
  )
}

#' @describeIn cond_proportion_j Description helper function for the conditional
#'   proportion summary label.
#'
#' @return
#' * `d_cond_proportion_j()` returns a string describing the analysis label.
#'
#' @export
d_cond_proportion_j <- function(conf_level, long = FALSE, num_limit, denom_limit, method = NULL) {
  label <- paste0(conf_level * 100, "% CI")

  if (long) {
    label <- paste(label, "for Response Rates")
  }

  method_part <- if (!is.null(method)) {
    switch(
      method,
      "wald" = "Wald",
      "clopper-pearson" = "Clopper-Pearson"
    )
  } else if (long) {
    paste0(
      "Wald if n >= ",
      denom_limit,
      " and x > ",
      num_limit,
      ", else Clopper-Pearson"
    )
  } else {
    "Wald / Clopper-Pearson"
  }

  paste0(label, " (", method_part, ")")
}

#' @describeIn cond_proportion_j Formatted analysis function which is used as
#'   `afun` for conditional proportion with adaptive CI selection (exact vs. Wald).
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
  .indent_mods = NULL,
  .df_row = NULL
) {
  dots_extra_args <- list(...)

  # Only support default stats, not custom stats
  .stats <- .split_std_from_custom_stats(.stats)$default_stats

  x_stats <- .apply_stat_functions(
    default_stat_fnc = s_cond_proportion_j,
    custom_stat_fnc_list = NULL,
    args_list = c(
      df = list(df),
      .var = .var,
      .df_row = list(.df_row),
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
