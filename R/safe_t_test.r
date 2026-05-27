#' @title Safe Wrapper for `stats::t.test()`
#'
#' @noRd
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A robust wrapper around [stats::t.test()] that prevents errors from
#' interrupting execution.
#'
#' Instead of failing, the function returns an object of class `"htest"`
#' containing `NA` values for inferential statistics together with the computed
#' sample mean(s) and an informative error message embedded in the `method`
#' field.
#'
#' This is particularly useful in pipelines, simulations, or batch analyses
#' where occasional invalid inputs (e.g., constant vectors or insufficient
#' observations) would otherwise stop execution.
#'
#' @details
#' When [stats::t.test()] succeeds, the resulting `"htest"` object is returned
#' unchanged except that `data.name` is reconstructed from the original
#' expressions supplied to `x` and `y`.
#'
#' When an error occurs, the function returns an object of class `"htest"` with:
#'
#' \itemize{
#'   \item `NA_real` values for `statistic`, `parameter`, `p.value`, `stderr`,
#'   and `conf.int`
#'   \item computed sample mean(s) stored in `estimate`
#'   \item the original null hypothesis value stored in `null.value`
#'   \item the original test type and error message appended to `method`.
#' }
#'
#' @inheritParams stats::t.test
#'
#' @return
#' An object of class `"htest"`.
#'
#' If [stats::t.test()] succeeds, the standard `"htest"` object is returned.
#' If an error occurs, an `"htest"` object with `NA` inferential statistics and
#' computed sample mean(s) in `estimate` is returned instead.
#'
#' @importFrom stats t.test
#'
#' @examples
#' # Standard usage
#' x <- 1:10
#' y <- 11:20
#'
#' t.test(x, y)
#' safe_t_test(x, y)
#'
#' # Paired test with constant differences
#' x <- rep(10, 5)
#'
#' t.test(x, x, paired = TRUE)
#' safe_t_test(x, x, paired = TRUE)
#'
#' # Example triggering an error (constant data)
#' t.test(x, x)
#' safe_t_test(x, x)
safe_t_test <- function(
    x,
    y = NULL,
    alternative = c("two.sided", "less", "greater"),
    mu = 0,
    paired = FALSE,
    var.equal = FALSE,
    conf.level = 0.95,
    ...) {
  checkmate::assert_numeric(x, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(y, finite = TRUE, any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_choice(alternative, choices = c("two.sided", "less", "greater"))
  checkmate::assert_number(mu, finite = TRUE)
  checkmate::assert_flag(paired)
  checkmate::assert_flag(var.equal)
  tern::assert_proportion_value(conf.level)

  x_expr <- substitute(x)
  y_expr <- substitute(y)
  alternative <- match.arg(alternative)
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

      res$data.name <- if (is.null(y)) {
        deparse1(x_expr)
      } else {
        paste(deparse1(x_expr), "and", deparse1(y_expr))
      }

      res
    },
    error = function(e) {
      if (paired) {
        x <- x - y
        y <- NULL
      }

      if (is.null(y)) {
        estimate <- c(mean(x, na.rm = TRUE))
        dname <- deparse1(x_expr)
        method <- ifelse(paired, "Paired t-test", "One Sample t-test")
        names(estimate) <- ifelse(paired, "mean difference", "mean of x")
      } else {
        estimate <- c(mean(x, na.rm = TRUE), mean(y, na.rm = TRUE))
        dname <- paste(deparse1(x_expr), "and", deparse1(y_expr))
        method <- paste(if (!var.equal) "Welch", "Two Sample t-test")
        names(estimate) <- c("mean of x", "mean of y")
      }
      estimate[is.nan(estimate)] <- NA_real_

      names(mu) <- if (paired) {
        "mean difference"
      } else if (!is.null(y)) {
        "difference in means"
      } else {
        "mean"
      }

      out <- list(
        statistic = setNames(NA_real_, "t"),
        parameter = setNames(NA_real_, "df"),
        p.value = NA_real_,
        conf.int = c(NA_real_, NA_real_),
        estimate = estimate,
        null.value = mu,
        stderr = NA_real_,
        alternative = alternative,
        method = paste0(method, " (failed: ", e$message, ")"),
        data.name = dname
      )
      attr(out$conf.int, "conf.level") <- conf.level

      class(out) <- "htest"
      out
    }
  )
}
