#' @title Summary Statistics for the Filtered Data.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Formatted analysis function for summary statistics for optionally
#' row-filtered data.
#' It uses [a_summary_j()] under the hood.
#'
#' @inheritParams proposal_argument_convention
#' @param subset_expr (`expression` or `NULL`)\cr
#'   logical expression indicating rows in `df` to keep for the analysis.
#'   Filtering is performed prior to analysis.
#' @param ... Additional arguments passed to [a_summary_j()]
#'
#' @returns an object created by [a_summary_j()] for a given input arguments.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   USUBJID = rep(1:6, each = 2),
#'   AVISIT = rep(c("Baseline", "Day 1"), 6),
#'   AVAL = c(1, 3, 2, 9, 13, 19, 15, 23, 43, 56, 24, 32)
#' ) |>
#'   mutate(ABLFL = AVISIT == "Baseline") |>
#'   group_by(USUBJID) |>
#'   mutate(
#'     BASE = AVAL[ABLFL],
#'     CHG = AVAL - BASE
#'   ) |>
#'   ungroup()
#' df <- as.data.frame(df)
#' df
#'
#' afun <- tern::a_summary
#' .stats <- c("n", "mean_sd")
#'
#' # No filter.
#' filter_df_prior_afun(dta_test, "CHG", afun, .stats = .stats)
#'
#' # Baseline records only.
#' filter_df_prior_afun(dta_test, "CHG", afun, expression(ABLFL), .stats = .stats)
filter_df_prior_afun <- function(df,
                                 .var,
                                 afun,
                                 subset_expr = expression(rep(TRUE, nrow(df))),
                                 ...) {
  checkmate::assert_data_frame(df)
  checkmate::assert_string(.var)
  checkmate::assert_choice(.var, colnames(df))
  checkmate::assert_function(afun)
  checkmate::assert_true(any(c("x", "df") %in% names(formals(afun))[1]))
  checkmate::assert_logical(with(df, eval(subset_expr)), len = nrow(df), any.missing = FALSE)

  # 1. Apply filter.
  row_mask <- with(df, eval(subset_expr))
  df <- df[row_mask, , drop = FALSE]

  # 2. Prepare arguments for afun.
  args_for_afun <- list(...)

  afun_formals <- names(formals(afun))

  if (".var" %in% afun_formals) {
    args_for_afun$.var <- .var
  }

  arg1 <- if (afun_formals[1] == "x") {
    list(x = df[, .var, drop = TRUE])
  } else {
    list(df = df)
  }
  args_for_afun <- c(arg1, args_for_afun)

  # 3. Call afun.
  do.call(afun, args_for_afun)
}
