#' @title Filter Data Prior To Analysis Function
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Applies row filtering to a dataset before executing a user-supplied analysis
#' function.
#'
#' @details
#' This is a generic wrapper that:
#' \enumerate{
#'   \item Subsets `df` using `subset_expr`.
#'   \item Passes data to `afun`, depending on its first argument. If it is named:
#'     \itemize{
#'       \item `x`, then `df[[.var]]` is passed.
#'       \item `df`, then the `df` data frame is passed.
#'     }
#'   \item Forwards `.var` (if it is present in the formal arguments of `afun`)
#'     and all additional arguments (`...`) to `afun`.
#' }
#'
#' @inheritParams proposal_argument_convention
#' @param afun (`function`)\cr Analysis function. Must accept `x` or `df` as
#'   its first parameter. Can optionally take other parameters.
#' @param subset_expr (`expression` or `NULL`)\cr
#'   Logical expression used to subset rows of `df` before analysis.
#'   Evaluated in the context of `df`.
#'   Defaults to `expression(rep(TRUE, nrow(df)))`, meaning no filtering.
#' @param ... Additional arguments passed to `afun`.
#'
#' @return
#' The object returned by `afun` applied to the filtered dataset.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   USUBJID = rep(1:6, each = 2),
#'   AVISIT = rep(c("Baseline", "Day 1"), 6),
#'   AVAL = c(1, 3, 2, 9, 13, 19, 15, 23, 43, 56, 24, 32),
#'   ABLFL = rep(c(TRUE, FALSE), 6),
#'   BASE = rep(c(1, 2, 13, 15, 43, 24), each = 2),
#'   CHG = c(0, 2, 0, 7, 0, 6, 0, 8, 0, 13, 0, 8)
#' )
#' df
#'
#' afun <- tern::a_summary
#' .stats <- c("n", "mean_sd")
#'
#' # No filtering.
#' filter_df_prior_afun(df, "CHG", afun, .stats = .stats)
#'
#' # Baseline records only.
#' filter_df_prior_afun(df, "CHG", afun, expression(ABLFL), .stats = .stats)
filter_df_prior_afun <- function(df,
                                 .var,
                                 afun,
                                 subset_expr = expression(rep(TRUE, nrow(df))),
                                 ...) {
  checkmate::assert_data_frame(df)
  checkmate::assert_string(.var)
  checkmate::assert_names(colnames(df), must.include = .var)
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
