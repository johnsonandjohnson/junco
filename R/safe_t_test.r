#' @title Safe Wrapper for `stats::t.test()`
#'
#' @noRd
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This is a robust wrapper around [stats::t.test.default()] that prevents
#' errors from interrupting execution. Instead of failing, it returns a
#' structured result containing `NA` values and an informative error message.
#'
#' This is particularly useful in pipelines, simulations, or batch analyses where
#' occasional invalid inputs (e.g., constant vectors, insufficient observations)
#' would otherwise stop execution.
#'
#' @details
#' When [stats::t.test.default()] succeeds, the result is returned unchanged.
#' If an error occurs, a list is returned mimicking key components of a `htest`
#' object.
#' Any `NaN` estimates are converted to `NA_real_`.
#'
#' @inheritParams stats::t.test
#'
#' @return
#' A `list`: either the standard `htest` object from [stats::t.test.default()],
#' or (on error) a list with `NA` statistics, sample mean(s) in `estimate`,
#' and an `error_text` field containing the error message.
#'
#' @importFrom stats t.test
#'
#' @examples
#' # Standard usage
#' t.test(1:10, 11:20)
#' safe_t_test(1:10, 11:20)
#'
#' # Example triggering failure (zero variance)
#' x <- rep(10, 5)
#'
#' stats::t.test(x, x)
#' safe_t_test(x, x)
#'
#' safe_t_test(x, x, paired = TRUE)
#'
safe_t_test <- function(
    x,
    y = NULL,
    alternative = c("two.sided", "less", "greater"),
    mu = 0,
    paired = FALSE,
    var.equal = FALSE,
    conf.level = 0.95,
    ...) {
  x_expr <- substitute(x)
  y_expr <- substitute(y)
  tryCatch(
    {
      res <- stats::t.test(
        x = x,
        y = y,
        alternative = alternative,
        mu = mu,
        paired = paired,
        var.equal = var.equal,
        conf.level = conf.level,
        ...
      )

      res$data.name <- if (!is.null(y)) {
        paste(deparse1(x_expr), "and", deparse1(y_expr))
      } else {
        deparse1(x_expr)
      }

      res
    },
    error = function(e) {
      if (is.null(y) || paired) {
        estimate <- c(mean_x = mean(x, na.rm = TRUE))
        dname <- deparse1(x_expr)
      } else {
        estimate <- c(
          mean_x = mean(x, na.rm = TRUE),
          mean_y = mean(y, na.rm = TRUE)
        )
        dname <- paste(deparse1(x_expr), "and", deparse1(y_expr))
      }
      estimate[is.nan(estimate)] <- NA_real_
      conf.int <- c(NA_real_, NA_real_)
      attr(conf.int, "conf.level") <- conf.level

      out <- list(
        statistic = NA_real_,
        parameter = NA_real_,
        p.value = NA_real_,
        estimate = estimate,
        conf.int = conf.int,
        stderr = NA_real_,
        method = "t-test (failed)",
        data.name = dname,
        error_text = e$message
      )

      class(out) <- "htest"

      out
    }
  )
}
