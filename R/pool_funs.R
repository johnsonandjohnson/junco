#' Pool a scalar estimate across imputations
#'
#' Applies Rubin's rules to combine scalar estimates and their
#' within-imputation variance estimates.
#'
#' @details For `m` imputations, the pooled estimate is the mean of `q`. The
#'   pooled variance combines the mean within-imputation variance with the
#'   between-imputation variance, using the usual `(1 + 1 / m)` multiplier.
#'   The returned standard error is the square root of the pooled variance.
#'
#' @param q (`numeric`)
#'   Vector of scalar estimates, with one value for each imputed dataset.
#' @param u (`numeric`)
#'   Vector of within-imputation variance estimates corresponding to `q`.
#'
#' @return A named `list` with the pooled estimate (`est`), standard error
#'   (`se`), variance (`var`), and number of imputations (`m`).
#'
#' @seealso [pool_z_stat()]
#' @export
#'
#' @examples
#' pool_rubin_scalar(
#'   q = c(0.20, 0.25, 0.15),
#'   u = c(0.01, 0.016, 0.012)
#' )
pool_rubin_scalar <- function(q, u) {
  # q: vector of estimates
  # u: vector of within-imputation variances
  m <- length(q)
  qbar <- mean(q, na.rm = TRUE)
  ubar <- mean(u, na.rm = TRUE)
  b <- stats::var(q, na.rm = TRUE)
  tvar <- ubar + (1 + 1 / m) * b
  se <- sqrt(tvar)
  list(est = qbar, se = se, var = tvar, m = m)
}

#' Pool z statistics across imputations
#'
#' Combines z statistics from multiple imputed datasets and calculates a
#' two-sided p-value for the pooled statistic.
#'
#' @details The within-imputation variance of a z statistic is approximately 1.
#'   The between-imputation variance is pooled using the same
#'   `(1 + 1 / m)` multiplier as [pool_rubin_scalar()]. The returned p-value is
#'   calculated from the standard normal distribution.
#'
#' @param z_stat_vals (`numeric`)
#'   Vector of z statistics, with one value for each imputed dataset.
#'
#' @return A named `list` with the pooled z statistic (`z`) and its two-sided
#'   p-value (`p`).
#'
#' @seealso [pool_rubin_scalar()]
#' @export
#'
#' @examples
#' pool_z_stat(c(1.1, 1.3, 1.2))
pool_z_stat <- function(z_stat_vals) {
  m <- length(z_stat_vals)
  qbar <- mean(z_stat_vals, na.rm = TRUE)
  b <- stats::var(z_stat_vals, na.rm = TRUE)
  ubar <- 1 # approx var(z) ~ 1 because it is a z statistic
  tvar <- ubar + (1 + 1 / m) * b
  z_pool <- qbar / sqrt(tvar)
  p_pool <- 2 * stats::pnorm(abs(z_pool), lower.tail = FALSE)
  list(z = z_pool, p = p_pool)
}
