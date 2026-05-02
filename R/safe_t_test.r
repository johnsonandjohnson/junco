#' @title Safe Wrapper of `stats::t.test()`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Handles errors when executing [stats::t.test()].
#'
#' @inheritParams stats::t.test
#'
#' @return A `list` with core items as from [stats:::t.test.default()] and
#'   `error_text` for the captured error.
#'
#' @importFrom stats t.test
#' @examples
#' t.test(rep(10, 5), rep(10, 5))
#' safe_t_test(rep(10, 5), rep(10, 5))
#'
safe_t_test <- function(x, y = NULL, ...) {
  tryCatch(
    stats::t.test(x, y, ...),
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
        method = "t-test (failed)",
        data.name = dname,
        error_text = e$message
      )
    }
  )
}
