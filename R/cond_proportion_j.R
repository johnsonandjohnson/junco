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
#' Depending on the `method_scope` choice, this decision is either taken
#' based on the response data for an individual cell (`method_scope = "cell"`),
#' or based on the response data for the entire row (`method_scope = "row"`):
#'
#' - With `method_scope = "cell"`, the counts used for the decision are:
#'   - numerator: the number of responders in the current cell;
#'   - denominator: specified by `denom` (`n` or `N_row`).
#' - With `method_scope = "row"`, the counts used for the decision are:
#'   - numerator: the total number of responders across the row (using `.df_row`);
#'   - denominator: `.N_row`, i.e. the total number of observations across the row.
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
#'   "n" (default, number of observed records), or "N_row" (number of observations in the row).
#' @param long (`flag`)\cr whether a long description is required.
#' @param na.rm (`flag`)\cr whether `NA` responses should be removed before analysis.
#'   If `FALSE` and `NA` values are present, an error is raised.
#'   Note that missing values are also removed from the row-wise counts
#'   which is relevant when `method_scope = "row"` or `denom = "N_row"`.
#' @param num_limit (`int`)\cr numerator limit to trigger the exact method.
#' @param denom_limit (`int`)\cr denominator limit to trigger the exact method.
#' @param method_scope (`string`)\cr select the CI method using counts from the
#'   current cell (`"cell"`) or all columns in the current row (`"row"`). See details.
#' @param .df_row (`data.frame`)\cr data for the current row across all columns,
#'   supplied by `rtables` when `method_scope = "row"`.
#' @param method (`string` or `NULL`)\cr selected CI method to show in the
#'   label. `NULL` retains the combined method description.
#' @param reason (`string` or `NULL`)\cr reason for selecting the CI method.
#'
#' @name cond_proportion_j
NULL

#' Helper Function to Extract Count Responders
#'
#' Converts a response vector, or a response column from a data frame, to logical
#' values and returns its responder and observation counts.
#'
#' @param x (`vector` or `data.frame`)\cr Response data.
#' @param .var (`string`)\cr Response column in `x` when `x` is a data frame.
#' @param na.rm (`flag`)\cr Whether to remove missing responses before counting.
#'
#' @return A named list with `rsp` (logical response vector), `n_rsp` (responders) and `len_rsp`
#'   (non-missing observations, when `na.rm = TRUE`).
#'
#' @keywords internal
h_get_rsp_counts <- function(x, .var, na.rm = FALSE) {
  rsp <- if (checkmate::test_atomic_vector(x)) {
    x
  } else {
    tern::assert_df_with_variables(x, list(rsp = .var))
    x[[.var]]
  }
  rsp <- safe_as_logical(rsp, na.rm = na.rm)
  list(rsp = rsp, n_rsp = sum(rsp), len_rsp = length(rsp))
}

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
#' # Using method_scope = "row" (requires .df_row in ...)
#' df_row <- data.frame(rsp = rep(rsp_v, 2))
#' s_cond_proportion_j(
#'   dta, .var = "rsp", method_scope = "row",
#'   .df_row = df_row
#' )
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
  .N_col,
  method_scope = c("cell", "row"),
  .df_row = NULL
) {
  checkmate::assert_flag(long)
  checkmate::assert_flag(na.rm)
  assert_proportion_value(conf_level)
  checkmate::assert_int(num_limit, lower = 0)
  checkmate::assert_int(denom_limit, lower = 0)
  denom <- match.arg(denom)
  method_scope <- match.arg(method_scope)

  rsp_counts <- h_get_rsp_counts(df, .var, na.rm = na.rm)
  rsp <- rsp_counts[["rsp"]]
  n_obs <- rsp_counts[["len_rsp"]]
  n_rsp <- rsp_counts[["n_rsp"]]

  if (method_scope == "row" || denom == "N_row") {
    tern::assert_df_with_variables(.df_row, list(rsp = .var))
    row_rsp_counts <- h_get_rsp_counts(.df_row, .var, na.rm = na.rm)
  }
  denom_val <- match.arg(denom) |>
    switch(
      n = n_obs,
      N_row = row_rsp_counts[["len_rsp"]]
    )
  assert_int(denom_val, lower = n_obs)
  p_hat <- ifelse(denom_val > 0, n_rsp / denom_val, 0)

  # The method is shared by all cells in a row when requested. The CI itself
  # still uses the current cell's responses and denominator. Therefore
  # we separate out here `method_denom` and `method_rsp` for the method decision.
  if (method_scope == "row") {
    method_denom <- row_rsp_counts[["len_rsp"]]
    method_rsp <- row_rsp_counts[["n_rsp"]]
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

  label <- d_cond_proportion_j(
    conf_level,
    long = long,
    num_limit = num_limit,
    denom_limit = denom_limit,
    method = if (method_scope == "row") method else NULL,
    method_denom = if (method_scope == "row") method_denom else NULL,
    method_rsp = if (method_scope == "row") method_rsp else NULL
  )

  list(
    "n_prop" = formatters::with_label(c(n_rsp, p_hat), "Responders"),
    "prop_ci" = formatters::with_label(
      x = 100 * prop_ci,
      label = label
    )
  )
}

#' @describeIn cond_proportion_j Description helper function for the conditional
#'   proportion summary label.
#'
#' @return
#' * `d_cond_proportion_j()` returns a string describing the analysis label.
#'
#' @examples
#' d_cond_proportion_j(conf_level = 0.90, long = FALSE, num_limit = 0, denom_limit = 10)
#'
#' # With dedicated row-wise method:
#' d_cond_proportion_j(
#'   conf_level = 0.90, long = TRUE, num_limit = 0, denom_limit = 10,
#'   method = "wald", reason = "n > 10, x > 0, x < n")
#'
#' @export
d_cond_proportion_j <- function(
  conf_level,
  long = FALSE,
  num_limit = NULL,
  denom_limit = NULL,
  method = NULL,
  reason = NULL,
  method_denom = NULL,
  method_rsp = NULL
) {
  assert_proportion_value(conf_level)
  assert_flag(long)
  assert_string(method, null.ok = TRUE)

  label <- paste0(conf_level * 100, "% CI")

  if (long) {
    label <- paste(label, "for Response Rates")
  }

  method_part <- if (!is.null(method)) {
    assert_choice(method, choices = c("wald", "clopper-pearson"))
    if (is.null(reason)) {
      assert_count(num_limit)
      assert_count(denom_limit)
      assert_count(method_denom)
      assert_count(method_rsp)
      reason <- if (method == "wald") {
        paste0("n >= ", denom_limit, ", x = ", method_rsp)
      } else if (method_denom < denom_limit) {
        paste0("n < ", denom_limit)
      } else if (method_rsp <= num_limit) {
        if (num_limit == 0) "x = 0" else paste0("x <= ", num_limit)
      } else if (method_rsp >= (method_denom - num_limit)) {
        if (num_limit == 0) "x = n" else paste0("x >= n - ", num_limit)
      } else {
        stop("Selected Clopper-Pearson method has no matching selection criterion.")
      }
    }
    assert_string(reason, null.ok = FALSE)
    if (long) {
      switch(
        method,
        "wald" = paste0(
          "Wald because ",
          reason
        ),
        "clopper-pearson" = paste0(
          "Clopper-Pearson because ",
          reason
        )
      )
    } else {
      switch(
        method,
        "wald" = "Wald",
        "clopper-pearson" = "Clopper-Pearson"
      )
    }
  } else if (long) {
    assert_count(num_limit)
    assert_count(denom_limit, positive = TRUE)
    paste0(
      "Wald if n >= ",
      denom_limit,
      ", x > ",
      num_limit,
      ", x < n - ",
      num_limit,
      "; else Clopper-Pearson"
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
  .N_col = NULL,
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
      .N_col = .N_col,
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
