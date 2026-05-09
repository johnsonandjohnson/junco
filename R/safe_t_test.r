#' @title Safe Wrapper for `stats::t.test()`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This is a robust wrapper around [stats::t.test] that prevents errors from
#' interrupting execution. Instead of failing, it returns a structured result
#' containing `NA` values and an informative error message.
#'
#' This is particularly useful in pipelines, simulations, or batch analyses where
#' occasional invalid inputs (e.g., constant vectors, insufficient observations)
#' would otherwise stop execution.
#'
#' @details
#' When [stats::t.test] succeeds, the result is returned unchanged.
#' If an error occurs, a list is returned mimicking key components of a `htest`
#' object.
#' Any `NaN` estimates are converted to `NA_real_`.
#'
#' @inheritParams stats::t.test
#'
#' @return
#' A `list`: either the standard `htest` object from [stats::t.test], or (on error)
#' a list with `NA` statistics, sample mean(s) in `estimate`, and an `error_text`
#' field containing the error message.
#'
#' @importFrom stats t.test
#'
#' @examples
#' # Standard usage
#' t.test(1:10, 11:20)
#' junco:::safe_t_test(1:10, 11:20)
#'
#' # Example triggering failure (zero variance)
#' \dontrun{
#' stats::t.test(rep(10, 5), rep(10, 5))
#' }
#' junco:::safe_t_test(rep(10, 5), rep(10, 5))
safe_t_test <- function(x, y = NULL, ...) {
  x_expr <- substitute(x)
  y_expr <- substitute(y)
  tryCatch(
    {
      res <- stats::t.test(x, y, ...)

      res$data.name <- if (!is.null(y)) {
        paste(deparse1(x_expr), "and", deparse1(y_expr))
      } else {
        deparse1(x_expr)
      }

      res
    },
    error = function(e) {
      if (!is.null(y)) {
        estimate <- c(mean_x = mean(x), mean_y = mean(y))
        dname <- paste(deparse1(substitute(x)), "and", deparse1(substitute(y)))
      } else {
        estimate <- c(mean_x = mean(x))
        dname <- deparse1(substitute(x))
      }
      estimate[is.nan(estimate)] <- NA_real_
      list(
        statistic = NA_real_,
        parameter = NA_real_,
        p.value = NA_real_,
        estimate = estimate,
        conf.int = c(NA_real_, NA_real_),
        stderr = NA_real_,
        method = "t-test (failed)",
        data.name = dname,
        error_text = e$message
      )
    }
  )
}
